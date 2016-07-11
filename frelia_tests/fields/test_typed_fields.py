import datetime

from frelia import fields


def test_bool_field_default_value(field_utils):
    obj = field_utils.obj_with_field(fields.BoolField())
    assert obj.field is False


def test_string_field_default_value(field_utils):
    obj = field_utils.obj_with_field(fields.StringField())
    assert obj.field == ''


def test_list_field_default_value(field_utils):
    field_class = field_utils.class_with_field(fields.ListField())
    obj1 = field_class()
    obj2 = field_class()
    assert obj1.field == obj2.field == []
    assert obj1.field is not obj2.field


def test_setting_date_time_field(field_utils):
    obj = field_utils.obj_with_field(fields.BaseDateTimeField())
    obj.field = datetime.date(2000, 1, 2)
    assert obj.field == datetime.datetime(2000, 1, 2)
    assert isinstance(obj.field, datetime.datetime)


def test_ensure_datetime_on_datetime():
    got = fields.BaseDateTimeField._ensure_datetime(datetime.datetime(2000, 1, 2))
    assert got == datetime.datetime(2000, 1, 2)
    assert isinstance(got, datetime.datetime)


def test_ensure_datetime_on_date():
    got = fields.BaseDateTimeField._ensure_datetime(datetime.date(2000, 1, 2))
    assert got == datetime.datetime(2000, 1, 2)
    assert isinstance(got, datetime.datetime)


def test_date_time_field_default_value(field_utils):
    obj = field_utils.obj_with_field(
        fields.DateTimeField(datetime.date(2000, 1, 2)))
    assert obj.field == datetime.datetime(2000, 1, 2)
    assert isinstance(obj.field, datetime.datetime)
