#!/usr/bin/python3
# This example runs a scenario, finds roads with high driver throughput, then
# establishes a per-hour cap.
#
# Before running this script, start the API server:
#
# > cargo run --release --bin headless -- --port=1234 --alerts=silence
#
# You may need to install https://requests.readthedocs.io
# Keep this script formatted with autopep8 -i

import argparse

from simulation import Simulation


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--api', default='http://localhost:1234')
    parser.add_argument('--country_code', default='us')
    parser.add_argument('--city_name', default='seattle')
    parser.add_argument('--map_name', default='montlake')
    parser.add_argument('--hours', type=int, default=128)

    args = parser.parse_args()

    print("Simulation Info:")
    print(f"\tMap: {args.country_code}/{args.city_name}/{args.map_name}")
    print(f"\tTime: {args.hours}")

    sim = Simulation(**vars(args))
    # sim.simulate(args.hours * 3600)
    sim.simulate(24 * 3600)
    sim.simulate(16 * 3600)
    sim.simulate(16 * 3600)
    sim.simulate(16 * 3600)
    sim.simulate(72 * 3600)

    print(f"\tTime Simulated in seconds: {sim.curr_time_delta_s()}")
    print(f"\t24h in seconds: {24 * 60 * 60}")

    trips = sim.finished_trips()
    print(f"Trips: {trips.dtypes}")

    agent_pos = sim.agent_positions()
    print(f"Agent Positions: {agent_pos.dtypes}")

    road_thruput = sim.road_thruput()
    print(f"Road Thruput: {road_thruput.dtypes}")

    blocked = sim.blocked_agents()
    print(f"Blocked Agents: {blocked}")

    all_trips = sim.total_time_lower_bound()
    print(f"Trips: {all_trips}")

    all_edits = sim.current_edits()
    print(f"Edits: {all_edits}")

    road_id = 4
    id_edits = sim.current_edits(road_id=road_id)
    print(f"Edits at id {road_id}: {id_edits}")

    all_geometry = sim.geometry()
    print(f"Geometry: {all_geometry}")

    intersection_id = 4
    id_geometry = sim.geometry(intersection_id=intersection_id)
    print(f"Geometry at id {intersection_id}: {id_geometry}")

    print("End")


if __name__ == "__main__":
    main()
