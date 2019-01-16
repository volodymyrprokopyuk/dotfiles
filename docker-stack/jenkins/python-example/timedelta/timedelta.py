from dateutil.parser import parse


HOURS_IN_DAY = 24
SECONDS_IN_HOUR = 3600


def time_delta(from_time_str, to_time_str):
    from_time = parse(from_time_str)
    to_time = parse(to_time_str)
    delta = to_time - from_time
    return delta


def time_delta_days(from_time_str, to_time_str):
    delta = time_delta(from_time_str, to_time_str)
    delta_days = delta.days
    return delta_days


def time_delta_hours(from_time_str, to_time_str):
    delta = time_delta(from_time_str, to_time_str)
    delta_days = delta.days
    delta_hours = delta_days * HOURS_IN_DAY + delta.seconds // SECONDS_IN_HOUR
    return delta_hours
