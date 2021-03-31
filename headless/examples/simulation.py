import requests

from typing import Any, Dict, List, Optional
import pandas as pd


class Simulation:

    __SECONDS_IN_A_DAY: int = 60 * 60 * 24

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

        curr_time = self.curr_time_s()
        # Don't write yet in case of fail
        new_curr_time_s = time_s + curr_time

        hours, minutes, seconds = new_curr_time_s // 3600, (new_curr_time_s // 60) % 60, new_curr_time_s % 60
        self.get("/sim/goto-time", params={"t": f"{hours}:{minutes}:{seconds}"})

    def curr_time_s(self) -> int:
        # Need to get rid of miliseconds
        h, m, s = self.get("/sim/get-time").content.decode().split(".")[0].split(":")
        return int(h) * 3600 + int(m) * 60 + int(s)

    def curr_time_delta_s(self) -> int:
        return self.curr_time_s() - self.start_time_s

    def finished_trips(self) -> pd.DataFrame:
        return pd.DataFrame(self.get("/data/get-finished-trips").json()).convert_dtypes(convert_string=True)

    def agent_positions(self) -> pd.DataFrame:
        result = self.get("/data/get-agent-positions").json()["agents"]
        if not result:
            return pd.DataFrame()
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
            data=self.get("/data/get-road-thruput").json()["counts"],
            columns=["id", "mode", "hour", "thruput"],
        ).convert_dtypes(convert_string=True)

    def blocked_agents(self) -> pd.DataFrame:
        result = self.get("/data/get-blocked-by-graph").json()["blocked_by"]
        print(f"Blocked Agents result: {result}")
        return pd.DataFrame(result)

    def trip_total_time_lower_bound(self, trip_id) -> int:
        result = self.get("/data/trip-time-lower-bound", id=trip_id)
        return int(result.content.decode())

    def total_time_lower_bound(self) -> pd.DataFrame:
        result = self.get("/data/all-trip-time-lower-bounds").json()
        return pd.DataFrame(list(result.values()), index=list(result.keys()), columns=[""])

    def current_edits(self, road_id: Optional[int] = None) -> Dict[str, Any]:
        if road_id is None:
            return self.get("/map/get-edits").json()
        else:
            return self.get("/map/get-edit-road-command", params={"id": road_id}).json()

    def geometry(self, intersection_id: Optional[int] = None):
        if intersection_id is None:
            result = self.get("/map/get-all-geometry").json()
        else:
            result = self.get("/map/get-intersection-geometry", params={"id": intersection_id}).json()

        result = result["features"]
        for idx, d in enumerate(result):
            properties = d["properties"]
            for k, v in properties.items():
                d[k] = v
            del d["properties"]
            geometry = d["geometry"]
            for k, v in geometry.items():
                d[k] = v
            del d["geometry"]

        # print(result.keys())
        return pd.DataFrame(result)

