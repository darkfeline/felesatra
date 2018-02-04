from google.appengine.ext import webapp
from google.appengine.ext.webapp.util import run_wsgi_app

class NotFoundPageHandler(webapp.RequestHandler):
    def get(self):
        self.error(404)
        self.response.out.write('<Your 404 error html page>')

application = webapp.WSGIApplication([('/.*', NotFoundPageHandler)],
                                     debug=True)

def main():
    run_wsgi_app(application)

if __name__ == "__main__":
    main()
