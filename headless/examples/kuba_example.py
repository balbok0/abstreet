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

import abst_helpers
import requests
import argparse
from datetime import datetime, timedelta, time
from typing import Any, Dict, List, Optional
import pandas as pd


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--api', default='http://localhost:1234')
    parser.add_argument('--country_code', default='us')
    parser.add_argument('--city_name', default='seattle')
    parser.add_argument('--map_name', default='montlake')
    parser.add_argument('--hours', type=int, default=16)

    args = parser.parse_args()

    print("Simulation Info:")
    print(f"\tMap: {args.country_code}/{args.city_name}/{args.map_name}")
    print(f"\tTime: {args.hours}")

    sim = Simulation(**vars(args))
    sim.simulate(args.hours * 3600)

    print(f"\tTime Simulated in seconds: {sim.curr_time_delta_s()}")

    trips = sim.finished_trips()
    # print(trips.dtypes)

    agent_pos = sim.agent_positions()
    # print(agent_pos.dtypes)

    road_thruput = sim.road_thruput()
    # print(road_thruput.dtypes)

    blocked = sim.blocked_agents()
    print(blocked)

    all_trips = sim.total_time_lower_bound()
    print(all_trips)

    print("End")


class Simulation:
    def __init__(
        self,
        api: str,
        scenario: Optional[str] = None,
        country_code: Optional[str] = None,
        city_name: Optional[str] = None,
        map_name: Optional[str] = None,
        *args, **kwargs,
    ):
        self.api: str = api

        self.scenario: Optional[str] = scenario
        if (
            scenario is None
            and country_code is not None
            and city_name is not None
            and map_name is not None
        ):
            self.scenario = self.__default_scenario_name(country_code, city_name, map_name)

        self.sim_started: bool = False
        self.start_time_s: int = 0  # in seconds

    @staticmethod
    def __default_scenario_name(
        country_code: Optional[str],
        city_name: Optional[str],
        map_name: Optional[str],
    ) -> Optional[str]:
        if (
            country_code is not None
            and city_name is not None
            and map_name is not None
        ):
            return f'data/system/{country_code}/{city_name}/scenarios/{map_name}/weekday.bin'
        else:
            return None

    def get(self, cmd, **kwargs):
        resp = requests.get(f"{self.api}{cmd}", **kwargs)
        if resp.status_code != requests.codes.ok:
            raise Exception(resp.text)
        return resp

    def post(self, cmd, **kwargs):
        resp = requests.post(f"{self.api}{cmd}", **kwargs)
        if resp.status_code != requests.codes.ok:
            raise Exception(resp.text)
        return resp

    def reset(
        self, scenario: Optional[str] = None, modifiers: List = [], edits: Dict[str, Any] = None
    ):
        if scenario is not None:
            self.scenario = scenario

        self.post(
            '/sim/load',
            json={
                'scenario': self.scenario,
                'modifiers': modifiers,
                'edits': edits,
            }
        )
        self.sim_started = True
        self.start_time_s = self.curr_time_s()  # This is count of seconds since simulation started

    def simulate(self, time_s: int):
        if not self.sim_started:
            self.reset()
        # Don't write yet in case of fail
        new_curr_time_s = time_s + self.curr_time_s()

        hours, minutes, seconds = new_curr_time_s // 3600, (new_curr_time_s // 60) % 60, new_curr_time_s % 60
        self.post("/sim/goto-time", params={"t": f"{hours}:{minutes}:{seconds}"})

    def curr_time_s(self) -> int:
        # Need to get rid of miliseconds
        h, m, s = self.get("/sim/get-time").content.decode().split(".")[0].split(":")
        return int(h) * 3600 + int(m) * 60 + int(s)

    def curr_time_delta_s(self) -> int:
        return self.curr_time_s() - self.start_time_s

    def finished_trips(self) -> pd.DataFrame:
        return pd.DataFrame(self.post("/data/get-finished-trips").json()).convert_dtypes(convert_string=True)

    def agent_positions(self) -> pd.DataFrame:
        result = self.post("/data/get-agent-positions").json()["agents"]
        for idx, d in enumerate(result):

            # Process mode
            _id = d["id"]
            mode = next(iter(_id.keys()))
            _val = _id[mode]
            d["mode"] = mode
            d["id"] = _val[0] if isinstance(_val, list) else _val  # Car is weird

            # Unwrap position
            for k, v in d["pos"].items():
                d[k] = v
            del d["pos"]

        return pd.DataFrame(result).convert_dtypes(convert_string=True)

    def road_thruput(self) -> pd.DataFrame:
        return pd.DataFrame(
            data=self.post("/data/get-road-thruput").json()["counts"],
            columns=["id", "mode", "hour", "thruput"],
        ).convert_dtypes(convert_string=True)

    def blocked_agents(self) -> pd.DataFrame:
        result = self.post("/data/get-blocked-by-graph").json()["blocked_by"]
        return pd.DataFrame(result)

    def trip_total_time_lower_bound(self, trip_id) -> int:
        result = self.post("/data/trip-time-lower-bound", id=trip_id)
        return int(result.content.decode())

    def total_time_lower_bound(self) -> pd.DataFrame:
        result = self.post("/data/all-trip-time-lower-bounds").json()
        return pd.DataFrame(list(result.values()), index=list(result.keys()), columns=[""])


if __name__ == "__main__":
    main()
