import frelia.enja
import frelia.jinja
import frelia.page
import frelia.transform


class Config:

    page_loader = frelia.page.PageLoader(frelia.enja.EnjaDocument)
    page_renderer = ...
    page_transform = frelia.transform.TransformGroup([
        frelia.transform.RenderJinja(frelia.jinja.Environment()),
    ])
    globals_dict = {}
    environment_options = {}
    ...
