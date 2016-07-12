from unittest import mock

import jinja2
import pytest

import frelia.page


def test_render(env, template, document):
    renderer = frelia.page.JinjaDocumentRenderer(env)
    got = renderer.render(document)
    assert got == mock.sentinel.output


def test_get_template_name_default(env, document):
    renderer = frelia.page.JinjaDocumentRenderer(
        env,
        default_template='base.html')
    got = renderer._get_template_name(document)
    assert got == 'base.html'


def test_get_template_name_explicit(env, document):
    renderer = frelia.page.JinjaDocumentRenderer(
        env,
        default_template='base.html')
    document.metadata['template'] = 'explicit.html'
    got = renderer._get_template_name(document)
    assert got == 'explicit.html'


def test_get_context(env, document):
    renderer = frelia.page.JinjaDocumentRenderer(env)
    got = renderer._get_context(document)
    assert got == {
        'sophie': 'prachta',
        'content': 'girl meets girl',
    }


def test_get_template(env, template, document):
    renderer = frelia.page.JinjaDocumentRenderer(env)
    document.metadata['template'] = 'explicit.html'
    got = renderer._get_template(document)
    assert got is template
    assert env.mock_calls == [mock.call.get_template('explicit.html')]


@pytest.fixture
def template():
    template = mock.NonCallableMock(['render'])
    template.render.return_value = mock.sentinel.output
    return template


@pytest.fixture
def env(template):
    env = mock.create_autospec(jinja2.Environment, instance=True)
    env.get_template.return_value = template
    return env
