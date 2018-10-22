// Copyright 2018 Google LLC, licensed under http://www.apache.org/licenses/LICENSE-2.0

use objects::{DEBUG_EXTRA, ID};
use piston::input::Key;
use plugins::{Plugin, PluginCtx};
use std::collections::HashSet;

pub struct Hider {
    items: HashSet<ID>,
}

impl Hider {
    pub fn new() -> Hider {
        Hider {
            items: HashSet::new(),
        }
    }

    pub fn show(&self, id: ID) -> bool {
        !self.items.contains(&id)
    }
}

impl Plugin for Hider {
    fn event(&mut self, ctx: PluginCtx) -> bool {
        let (input, selected) = (ctx.input, &mut ctx.primary.current_selection);

        if input.unimportant_key_pressed(Key::K, DEBUG_EXTRA, "unhide everything") {
            info!("Unhiding {} things", self.items.len());
            self.items.clear();
            return true;
        }

        let item = match selected {
            // No real use case for hiding moving stuff
            Some(ID::Car(_)) => {
                return false;
            }
            Some(ID::Pedestrian(_)) => {
                return false;
            }
            Some(id) => id.clone(),
            _ => {
                return false;
            }
        };
        if input.unimportant_key_pressed(Key::H, DEBUG_EXTRA, &format!("hide {:?}", item)) {
            self.items.insert(item);
            info!("Hiding {:?}", item);
            *selected = None;
            return true;
        }
        false
    }
}
