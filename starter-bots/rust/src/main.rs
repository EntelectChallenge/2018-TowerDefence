extern crate serde;
extern crate serde_json;

#[macro_use]
extern crate serde_derive;

extern crate rand;
use rand::{thread_rng, Rng};

use std::error::Error;

#[repr(u8)]
#[derive(Debug, Clone, Copy)]
enum Building {
    Defense = 0,
    Attack = 1,
    Energy = 2,
}

impl std::fmt::Display for Building {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", *self as u8)
    }
}

const STATE_PATH: &str = "state.json";

const COMMAND_PATH: &str = "command.txt";

#[derive(Debug, Clone, Copy)]
struct Command {
    x: u32,
    y: u32,
    building: Building,
}

impl std::fmt::Display for Command {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{},{},{}", self.x, self.y, self.building)
    }
}

mod state;

fn current_energy(state: &state::State) -> u32 {
    state.players.iter()
        .filter(|p| p.player_type == 'A')
        .map(|p| p.energy)
        .next()
        .unwrap_or(0)
}

fn can_afford_all_buildings(state: &state::State) -> bool {
    can_afford_attack_buildings(state) &&
        can_afford_defence_buildings(state) &&
        can_afford_energy_buildings(state)
}

fn can_afford_attack_buildings(state: &state::State) -> bool {
    current_energy(state) >= state.game_details.building_prices.attack
}
fn can_afford_defence_buildings(state: &state::State) -> bool {
    current_energy(state) >= state.game_details.building_prices.defense
}
fn can_afford_energy_buildings(state: &state::State) -> bool {
    current_energy(state) >= state.game_details.building_prices.energy
}

fn is_under_attack(state: &state::State, y: u32) -> bool {
    let attack = state.game_map[y as usize].iter()
        .any(|cell| cell.buildings.iter()
             .any(|building| building.player_type == 'B' &&
                  building.building_type == "ATTACK"));
    let defences = state.game_map[y as usize].iter()
        .any(|cell| cell.buildings.iter()
             .any(|building| building.player_type == 'A' &&
                  building.building_type == "DEFENSE"));
    attack && !defences
}

fn is_occupied(state: &state::State, x: u32, y: u32) -> bool {
    !state.game_map[y as usize][x as usize].buildings.is_empty()
}

fn unoccupied_in_row(state: &state::State, y: u32) -> Vec<u32> {
    (0..state.game_details.map_width/2)
        .filter(|&x| !is_occupied(&state, x, y))
        .collect()
}

fn unoccupied_cells(state: &state::State) -> Vec<(u32, u32)> {
    (0..state.game_details.map_width/2)
        .flat_map(|x| (0..state.game_details.map_height)
                  .map(|y| (x, y))
                  .collect::<Vec<(u32, u32)>>())
        .filter(|&(x, y)| !is_occupied(&state, x, y))
        .collect()
}

fn choose_move(state: &state::State) -> Option<Command> {
    let mut rng = thread_rng();

    if can_afford_defence_buildings(state) {
        for y in 0..state.game_details.map_height {
            if is_under_attack(state, y) {
                let x_options = unoccupied_in_row(state, y);
                if let Some(&x) = rng.choose(&x_options) {
                    return Some(Command {
                        x: x,
                        y: y,
                        building: Building::Defense
                    });
                }
            }
        }
    }

    if can_afford_all_buildings(state) {
        let options = unoccupied_cells(state);
        let option = rng.choose(&options);
        let buildings = [Building::Attack, Building::Defense, Building::Energy];
        let building = rng.choose(&buildings);
        match (option, building) {
            (Some(&(x, y)), Some(&building)) => Some(Command {
                x: x,
                y: y,
                building: building
            }),
            _ => None
        }
    }
    else {
        None
    }
}

use std::fs::File;
use std::io::prelude::*;

fn write_command(filename: &str, command: Option<Command>) -> Result<(), Box<Error> > {
    let mut file = File::create(filename)?;
    if let Some(command) = command {
        write!(file, "{}", command)?;
    }

    Ok(())
}

use std::process;

fn main() {
    let state = match state::read_state_from_file(STATE_PATH) {
        Ok(state) => state,
        Err(error) => {
            eprintln!("Failed to read the {} file. {}", STATE_PATH, error);
            process::exit(1);
        }
    };
    let command = choose_move(&state);

    match write_command(COMMAND_PATH, command) {
        Ok(()) => {}
        Err(error) => {
            eprintln!("Failed to write the {} file. {}", COMMAND_PATH, error);
            process::exit(1);
        }
    }
}
