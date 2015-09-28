---
contents:
- news
date: 2015-09-27
modified: 2015-09-27
publishdate: 2015-09-27
subjects:
- meta
title: HTTPS Update
---

Here's some news on recent site updates.

- I added HTTPS support.
- HTTPS uses a self-signed certificate.  Never fear, though, the certificate can
  be found on the [about page]({{< absURL "document/about" >}}), signed with my
  key, providing both secure encryption and trust.
- HTTPS uses TLS 1.2 as of this post, sidestepping the vulnerabilities in SSLv2
  and v3.
- HTTP redirects to HTTPS, so you won't accidentally browse without encryption
  or trust.
- Caching headers have been added, which means your browser will cache pages
  from this site and reduce your bandwidth usage.  Probably.
- Pages are all HTML5 compliant.  Yay!

Due to the unsigned certificate, your browser will most likely panic about
unsecured connections.  If this bugs you, you should download the certificate,
signature, and my public key from the above link, verify the signature, and then
add the certificate to your browser as a trusted authority.

Also, pages have been moved around, changing their canonical URLs.  I apologize
for the inconvenience, but it's for the best.
