import datetime

import pytest

from frelia import fields


class _DateTimeClass:
    field = fields.DateTimeField()


@pytest.fixture
def datetimeclass():
    return _DateTimeClass


def test_setting_date_time_field(datetimeclass):
    obj = datetimeclass()
    obj.field = datetime.date(2000, 1, 2)
    assert obj.field == datetime.datetime(2000, 1, 2)
    assert isinstance(obj.field, datetime.datetime)


def test_ensure_datetime_on_datetime():
    got = fields.DateTimeField._ensure_datetime(datetime.datetime(2000, 1, 2))
    assert got == datetime.datetime(2000, 1, 2)
    assert isinstance(got, datetime.datetime)


def test_ensure_datetime_on_date():
    got = fields.DateTimeField._ensure_datetime(datetime.date(2000, 1, 2))
    assert got == datetime.datetime(2000, 1, 2)
    assert isinstance(got, datetime.datetime)
