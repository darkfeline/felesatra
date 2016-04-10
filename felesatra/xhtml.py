"""Extended html parsing.

This isn't what would normally be considered XHTML, but rather a
felesatra-specific extended version of HTML5.

"""

from html.parser import HTMLParser
from itertools import chain


def _pop_stack(stack, item):
    """Pop item and everything before it off stack."""
    i = stack.index(item)
    del stack[i:]


class XHTMLParser(HTMLParser):

    """HTML parser extended with custom markup support."""

    # pylint: disable=abstract-method

    def reset(self):
        super().reset()
        self.text = []
        self._output_stack = [self.text]

        self._footnote_ids = []
        self._footnotes = {}
        self._footnote_stack = []

    def _output(self, text):
        """Output text during processing."""
        self._output_stack[-1].append(text)

    _fn_ref_template = '<sup><a href="#fn{0}" id="r{0}">[{0}]</a></sup>'

    def _fn_ref_start(self, tag, attrs):
        """Handle x-fn-ref."""
        # pylint: disable=unused-argument
        attrs = dict(attrs)
        fn_id = attrs['id']
        self._footnote_ids.append(fn_id)
        html_id = len(self._footnote_ids)
        self._output(self._fn_ref_template.format(html_id))

    def _fn_start(self, tag, attrs):
        """Handle x-fn."""
        # pylint: disable=unused-argument
        attrs = dict(attrs)
        fn_id = attrs['id']
        footnote = []
        self._footnotes[fn_id] = footnote
        self._output_stack.append(footnote)
        self._footnote_stack.append(footnote)

    def _default_start(self, tag, attrs):
        """Handle regular start tag."""
        attrs_text = ''.join(
            ' {}="{}"'.format(name, value)
            for name, value in attrs)
        self._output('<{}>'.format(tag + attrs_text))

    _starttag_handlers = {
        'x-fn-ref': _fn_ref_start,
        'x-fn': _fn_start,
        'default': _default_start,
    }

    def handle_starttag(self, tag, attrs):
        handler = self._starttag_handlers.get(
            tag, self._starttag_handlers['default'])
        handler(self, tag, attrs)

    def handle_data(self, data):
        self._output(data)

    def _fn_end(self, tag):
        """Handle x-fn endtag."""
        # pylint: disable=unused-argument
        footnote = self._footnote_stack.pop()
        _pop_stack(self._output_stack, footnote)

    def _default_end(self, tag):
        """Default endtag handler."""
        self._output('</{}>'.format(tag))

    _endtag_handlers = {
        'x-fn-ref': lambda self, tag: None,
        'x-fn': _fn_end,
        'default': _default_end,
    }

    def handle_endtag(self, tag):
        handler = self._endtag_handlers.get(
            tag, self._endtag_handlers['default'])
        handler(self, tag)

    _fn_block_template = '<section class="footnotes">{}</section>'
    _fn_template = '<p id="fn{0}"><a href="#r{0}">[{0}]</a> {1}</p>'

    def _get_footnote(self, i, key):
        """Get text for one footnote.

        i is the footnote index (for rendering), key is the internal footnote
        key.

        """
        if key not in self._footnotes:
            raise MissingRefError(key)
        return self._fn_template.format(i, ''.join(self._footnotes[key]))

    def _get_footnotes(self):
        """Get text for all footnotes."""
        return ''.join(
            self._get_footnote(i, key)
            for i, key in enumerate(self._footnote_ids, start=1))

    def get_text(self):
        """Get processed text."""
        return ''.join(chain(
            self.text,
            [self._fn_block_template.format(self._get_footnotes())]))


class ParseError(Exception):
    """Generic XHTML parse error."""


class MissingRefError(ParseError):

    """Missing footnote reference."""

    def __init__(self, key):
        super().__init__('Missing footnote reference {}'.format(key))
