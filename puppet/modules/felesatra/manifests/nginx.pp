class felesatra::nginx {
  package { 'nginx':
    ensure => latest,
  }

  service { 'nginx':
    ensure => running,
    enable => true,
  }
}
