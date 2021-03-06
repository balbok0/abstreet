//! This is a prototype of a total revamp to pathfinding, for
//! https://github.com/a-b-street/abstreet/issues/555. The basic idea is to have a very high-level
//! plan with way less functionality (like no modifying steps or tracking progress). There will
//! then be methods to transform that into specific lanes/turns, as needed by the sim and UI.

use std::collections::VecDeque;
use std::fmt;

use serde::{Deserialize, Serialize};

use geom::Distance;

use crate::{BuildingID, DirectedRoadID, Map, PathConstraints, Position};

/// A high-level step along a path. Turns / intersections are not captured, because they're
/// implied. This representation should closely (maybe exactly) match the internal representation
/// used in the contraction hierarchies.
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum PathStepV2 {
    /// Original direction
    Along(DirectedRoadID),
    /// Sidewalks only!
    Contraflow(DirectedRoadID),
    /// Always original direction, since this is only for vehicles. The sequence starts at the
    /// turn to enter the first road, and ends on the turn leaving the last.
    UberTurn(Vec<DirectedRoadID>),
}

/// A high-level path to follow. This can be transformed into more specific directions as needed.
/// It's mostly not modifiable (except for appending new steps, for parking) and doesn't track
/// distance and stuff.
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct PathV2 {
    steps: VecDeque<PathStepV2>,
    // The original request used to produce this path. Calling shift() and add() will NOT affect
    // this.
    orig_req: PathRequestV2,
}

impl PathV2 {
    pub(crate) fn new(steps: Vec<PathStepV2>, orig_req: PathRequestV2) -> PathV2 {
        // TODO Port validate_continuity and validate_restrictions?
        PathV2 {
            steps: VecDeque::from(steps),
            orig_req,
        }
    }

    /// The original PathRequestV2 used to produce this path. If the path has been modified since
    /// creation, the start and end of the request won't match up with the current path steps.
    pub fn get_req(&self) -> &PathRequestV2 {
        &self.orig_req
    }

    pub fn is_empty(&self) -> bool {
        self.steps.is_empty()
    }

    pub fn is_last_step(&self) -> bool {
        self.steps.len() == 1
    }

    pub fn isnt_last_step(&self) -> bool {
        self.steps.len() > 1
    }

    /// Advances along the path by one step. Returns the step just completed.
    pub fn shift(&mut self) -> PathStepV2 {
        self.steps.pop_front().unwrap()
    }

    /// Adds a step to the end of the path. We don't check validity/continuity; the caller must
    /// pass in something good.
    pub fn add(&mut self, step: PathStepV2) {
        self.steps.push_back(step);
    }

    pub fn current_step(&self) -> &PathStepV2 {
        &self.steps[0]
    }

    pub fn next_step(&self) -> &PathStepV2 {
        &self.steps[1]
    }
    pub fn maybe_next_step(&self) -> Option<&PathStepV2> {
        if self.is_last_step() {
            None
        } else {
            Some(self.next_step())
        }
    }

    pub fn last_step(&self) -> &PathStepV2 {
        &self.steps[self.steps.len() - 1]
    }

    pub fn get_steps(&self) -> &VecDeque<PathStepV2> {
        &self.steps
    }
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct PathRequestV2 {
    pub start: PositionV2,
    pub end: PositionV2,
    pub constraints: PathConstraints,
}

impl PathRequestV2 {
    /// Determines the start and end position to travel between two buildings for a certain mode.
    /// The path won't cover modality transfers -- if somebody has to walk between the building and
    /// a parking spot or bikeable position, that won't be captured here.
    pub fn between_buildings(
        map: &Map,
        from: BuildingID,
        to: BuildingID,
        constraints: PathConstraints,
    ) -> Option<PathRequestV2> {
        let from = map.get_b(from);
        let to = map.get_b(to);
        let (start, end) = match constraints {
            PathConstraints::Pedestrian => (from.sidewalk_pos, to.sidewalk_pos),
            PathConstraints::Bike => (from.biking_connection(map)?.0, to.biking_connection(map)?.0),
            PathConstraints::Car => (
                from.driving_connection(map)?.0,
                to.driving_connection(map)?.0,
            ),
            // These two aren't useful here. A pedestrian using transit would pass in
            // PathConstraints::Pedestrian. There's no reason yet to find a route for a bus or
            // train to travel between buildings.
            PathConstraints::Bus | PathConstraints::Train => unimplemented!(),
        };
        Some(PathRequestV2 {
            start: start.to_v2(map),
            end: end.to_v2(map),
            constraints,
        })
    }
}

impl fmt::Display for PathRequestV2 {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "PathRequestV2({} along {}... to {} along {} for {:?})",
            self.start.dist_along(),
            self.start.road(),
            self.end.dist_along(),
            self.end.road(),
            self.constraints,
        )
    }
}

/// Represents some distance along a directed road.
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct PositionV2 {
    // Don't let callers construct a PositionV2 directly, so it's easy to find callers of new().
    dr: DirectedRoadID,
    dist_along: Distance,
}

impl PositionV2 {
    pub fn new(dr: DirectedRoadID, dist_along: Distance) -> PositionV2 {
        PositionV2 { dr, dist_along }
    }

    pub fn road(&self) -> DirectedRoadID {
        self.dr
    }

    pub fn dist_along(&self) -> Distance {
        self.dist_along
    }
}

impl Position {
    // TODO Do we need to transform distances at all?
    pub fn to_v2(&self, map: &Map) -> PositionV2 {
        PositionV2::new(
            map.get_l(self.lane()).get_directed_parent(map),
            self.dist_along(),
        )
    }
}
