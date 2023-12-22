# Maintainer: Bram Neijt <bram@neijt.nl>
pkgname=after
pkgver=0.1.1
pkgrel=1
makedepends=('rust' 'cargo')
arch=('i686' 'x86_64' 'armv6h' 'armv7h')
pkgdesc="A tool wait for other processes to stop"
url="https://github.com/bneijt/after"

build() {
   cargo build --release --locked --all-features
}

package() {
  install -Dm 755 ../target/release/${pkgname} -t "${pkgdir}/usr/bin"
}