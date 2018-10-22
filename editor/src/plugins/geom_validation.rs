use generator;
use geo;
use geo::prelude::Intersects;
use geom::Polygon;
use objects::{DEBUG, ID};
use piston::input::Key;
use plugins::{Plugin, PluginCtx};
use render::DrawMap;

// Eventually this should be part of an interactive map fixing pipeline. Find problems, jump to
// them, ask for the resolution, record it.
pub enum Validator {
    Inactive,
    Active {
        gen: generator::Generator<'static, (), (ID, ID)>,
        current_problem: Option<(ID, ID)>,
    },
}

impl Validator {
    pub fn new() -> Validator {
        Validator::Inactive
    }

    fn start(draw_map: &DrawMap) -> Validator {
        let mut objects: Vec<(ID, Vec<geo::Polygon<f64>>)> = Vec::new();
        for l in &draw_map.lanes {
            objects.push((ID::Lane(l.id), make_polys(&l.polygon)));
        }
        for i in &draw_map.intersections {
            objects.push((ID::Intersection(i.id), make_polys(&i.polygon)));
        }
        for b in &draw_map.buildings {
            objects.push((ID::Building(b.id), make_polys(&b.fill_polygon)));
        }
        for p in &draw_map.parcels {
            objects.push((ID::Parcel(p.id), make_polys(&p.fill_polygon)));
        }

        info!(
            "{} objects total. About {} possible overlaps",
            objects.len(),
            objects.len().pow(2)
        );

        // TODO scoped vs unscoped?
        let gen = generator::Gn::<()>::new_scoped(move |mut s| {
            // TODO use a quadtree to prune
            for (id1, ls1) in &objects {
                for (id2, ls2) in &objects {
                    // Overlaps are symmetric and we're not worried about self-intersection, so only
                    // check when id1 < id2.
                    if id1 >= id2 {
                        continue;
                    }
                    // Buildings and parcels are expected to overlap.
                    match (id1, id2) {
                        (ID::Building(_), ID::Parcel(_)) => continue,
                        (ID::Parcel(_), ID::Building(_)) => continue,
                        _ => {}
                    };

                    'outer: for poly1 in ls1 {
                        for poly2 in ls2 {
                            if poly1.intersects(poly2) {
                                s.yield_((*id1, *id2));
                                break 'outer;
                            }
                        }
                    }
                }
            }
            done!();
        });

        Validator::Active {
            gen,
            current_problem: None,
        }
    }
}

impl Plugin for Validator {
    fn event(&mut self, ctx: PluginCtx) -> bool {
        let (input, canvas, map, sim, draw_map) = (
            ctx.input,
            ctx.canvas,
            &ctx.primary.map,
            &ctx.primary.sim,
            &ctx.primary.draw_map,
        );

        let mut new_state: Option<Validator> = None;
        match self {
            Validator::Inactive => {
                if input.unimportant_key_pressed(Key::I, DEBUG, "Validate map geometry") {
                    new_state = Some(Validator::start(draw_map));
                }
            }
            Validator::Active {
                gen,
                current_problem,
            } => {
                // Initialize or advance?
                if !current_problem.is_some() || input.key_pressed(Key::N, "see the next problem") {
                    // TODO do this in a bg thread or something
                    *current_problem = gen.next();

                    if let Some((id1, id2)) = current_problem {
                        info!("{:?} and {:?} intersect", id1, id2);
                        canvas.center_on_map_pt(id1.canonical_point(map, sim, draw_map).unwrap());
                    // TODO also modify selection state to highlight stuff?
                    } else {
                        info!("No more problems!");
                        new_state = Some(Validator::Inactive);
                    }
                } else if input.key_pressed(Key::Escape, "stop looking at problems") {
                    new_state = Some(Validator::Inactive);
                }
            }
        };
        if let Some(s) = new_state {
            *self = s;
        }
        match self {
            Validator::Inactive => false,
            _ => true,
        }
    }
}

fn make_polys(p: &Polygon) -> Vec<geo::Polygon<f64>> {
    let mut result = Vec::new();
    for tri in &p.triangles {
        let exterior = vec![
            geo::Point::new(tri.pt1.x(), tri.pt1.y()),
            geo::Point::new(tri.pt2.x(), tri.pt2.y()),
            geo::Point::new(tri.pt3.x(), tri.pt3.y()),
        ];
        result.push(geo::Polygon::new(exterior.into(), Vec::new()));
    }
    result
}
