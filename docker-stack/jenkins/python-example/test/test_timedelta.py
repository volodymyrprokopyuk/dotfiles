import pytest
# from pytest.mark import parametrize
from timedelta import time_delta_days


time_delta_days_test_data = [
    ("2019-01-16 12:34:56", "2019 Jan 16 12:34:56", 0),
    ("2019-01-16 12:34:56", "2019 Jan 17 12:34:56", 1),
    ("2019-01-17 12:34:56", "2019 Jan 16 12:34:56", -1)
]


@pytest.mark.parametrize("from_time_str, to_time_str, expected_delta_days", time_delta_days_test_data)
def test_time_delta_days(from_time_str, to_time_str, expected_delta_days):
    delta_days = time_delta_days(from_time_str, to_time_str)
    assert delta_days == expected_delta_days
