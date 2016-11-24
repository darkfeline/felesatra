class felesatra::felesatra {
  package { 'nginx':
    ensure => latest,
  }

  service { 'nginx':
    ensure => running,
    enable => true,
    require => Package['nginx'],
  }

  $confdir = '/etc/nginx'

  file { "${confdir}/sites-available/felesatra":
    source => 'puppet:///modules/felesatra/nginx-sites/felesatra',
    notify => Service['nginx'],
  }

  file { "${confdir}/sites-enabled/felesatra":
    ensure => link,
    target => '../sites-available/felesatra',
    notify => Service['nginx'],
  }

  file { "${confdir}/sites-enabled/default":
    ensure => missing,
    notify => Service['nginx'],
  }
}
