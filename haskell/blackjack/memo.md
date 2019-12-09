gi-gtk-declarativeを導入したらビルドができなくなった件について

# 環境

||||
|:-:|:-:|
| OS                                    | macOS Mojave 10.14.6                                    |
| stack version                         | Version 2.1.3 x86_64                                    |
| `pkg-config --modversion gobject-2.0` | 2.62.3                                                  |
| Source                                | [cj-bc/playground](https://github.com/cj-bc/playground) |
| commit                                | [c0fafa7ddf24d6c3c21d427e83d415efa5ae3a3a](https://github.com/Cj-bc/playground/tree/c0fafa7ddf24d6c3c21d427e83d415efa5ae3a3a/haskell/blackjack) |
| installed packages                    | `gtk+`/`gtk+3`/`gtk-mac-integration`/`gtksourceview3`/`gobject-introspection` from homembrew |

## インストール済みパッケージのバージョン

||||
|:-:|:-:|
| gtk+                  | 2.24.32 |
| gtk+3                 | 3.23.12 |
| gtk-mac-integration   | 2.1.3   |
| gtksourceview3        | 2.10.5  |
| gobject-introspection | 1.16.2  |



# 問題

既存のプロジェクトに`gi-gtk-declarative`及び`gi-gtk-declarative-app-simple`を追加したらビルドが失敗した

## 再現手順および結果


```sh
<@_@>:blackjack$ stack clean --full
$?is0(vim)<ttys010>
<X_X>:blackjack$ la
.               ..              .gitignore      .swp            BlackJack.cabal ChangeLog.md    LICENSE         README.md       Setup.hs        app             dist-newstyle   package.yaml    src             stack.yaml      stack.yaml.lock test            typescript
$?is0(vim)<ttys010>
<X_X>:blackjack$ stack build
gi-pango                     > configure
gi-pango                     > [1 of 2] Compiling Main             ( /private/var/folders/f4/_n6jxvsd1wd5cck49ntypshc0000gn/T/stack-1d4a2313b5fb120c/gi-pango-1.0.19/Setup.hs, /private/var/folders/f4/_n6jxvsd1wd5cck49ntypshc0000gn/T/stack-1d4a2313b5fb120c/gi-pango-1.0.19/.stack-work/dist/x86_64-osx/Cabal-2.4.0.1/setup/Main.o )
gi-pango                     > [2 of 2] Compiling StackSetupShim   ( /Users/me/.ghq/github.com/me/dotfiles/dotfiles/stack/setup-exe-src/setup-shim-mPHDZzAJ.hs, /private/var/folders/f4/_n6jxvsd1wd5cck49ntypshc0000gn/T/stack-1d4a2313b5fb120c/gi-pango-1.0.19/.stack-work/dist/x86_64-osx/Cabal-2.4.0.1/setup/StackSetupShim.o )
gi-pango                     > Linking /private/var/folders/f4/_n6jxvsd1wd5cck49ntypshc0000gn/T/stack-1d4a2313b5fb120c/gi-pango-1.0.19/.stack-work/dist/x86_64-osx/Cabal-2.4.0.1/setup/setup ...
gi-pango                     > Configuring gi-pango-1.0.19...
gi-pango                     > setup: An 'autogen-module' is neither on 'exposed-modules' or 'other-modules'.
gi-pango                     >
--  While building package gi-pango-1.0.19 using:
      /private/var/folders/f4/_n6jxvsd1wd5cck49ntypshc0000gn/T/stack-1d4a2313b5fb120c/gi-pango-1.0.19/.stack-work/dist/x86_64-osx/Cabal-2.4.0.1/setup/setup
          --builddir=.stack-work/dist/x86_64-osx/Cabal-2.4.0.1 configure
          --user
          --package-db=clear
          --package-db=global
          --package-db=/Users/me/.ghq/github.com/Cj-bc/dotfiles/dotfiles/stack/snapshots/x86_64-osx/2ba4b137dcf81e9b0f6e6bef8bcec08ab13e09ed94ebdfa1d7869c606593e53a/8.6.5/pkgdb
          --libdir=/Users/me/.ghq/github.com/Cj-bc/dotfiles/dotfiles/stack/snapshots/x86_64-osx/2ba4b137dcf81e9b0f6e6bef8bcec08ab13e09ed94ebdfa1d7869c606593e53a/8.6.5/lib
          --bindir=/Users/me/.ghq/github.com/Cj-bc/dotfiles/dotfiles/stack/snapshots/x86_64-osx/2ba4b137dcf81e9b0f6e6bef8bcec08ab13e09ed94ebdfa1d7869c606593e53a/8.6.5/bin
          --datadir=/Users/me/.ghq/github.com/Cj-bc/dotfiles/dotfiles/stack/snapshots/x86_64-osx/2ba4b137dcf81e9b0f6e6bef8bcec08ab13e09ed94ebdfa1d7869c606593e53a/8.6.5/share
          --libexecdir=/Users/me/.ghq/github.com/Cj-bc/dotfiles/dotfiles/stack/snapshots/x86_64-osx/2ba4b137dcf81e9b0f6e6bef8bcec08ab13e09ed94ebdfa1d7869c606593e53a/8.6.5/libexec
          --sysconfdir=/Users/me/.ghq/github.com/Cj-bc/dotfiles/dotfiles/stack/snapshots/x86_64-osx/2ba4b137dcf81e9b0f6e6bef8bcec08ab13e09ed94ebdfa1d7869c606593e53a/8.6.5/etc
          --docdir=/Users/me/.ghq/github.com/Cj-bc/dotfiles/dotfiles/stack/snapshots/x86_64-osx/2ba4b137dcf81e9b0f6e6bef8bcec08ab13e09ed94ebdfa1d7869c606593e53a/8.6.5/doc/gi-pango-1.0.19
          --htmldir=/Users/me/.ghq/github.com/Cj-bc/dotfiles/dotfiles/stack/snapshots/x86_64-osx/2ba4b137dcf81e9b0f6e6bef8bcec08ab13e09ed94ebdfa1d7869c606593e53a/8.6.5/doc/gi-pango-1.0.19
          --haddockdir=/Users/me/.ghq/github.com/Cj-bc/dotfiles/dotfiles/stack/snapshots/x86_64-osx/2ba4b137dcf81e9b0f6e6bef8bcec08ab13e09ed94ebdfa1d7869c606593e53a/8.6.5/doc/gi-pango-1.0.19
          --dependency=Cabal=Cabal-2.4.1.0-4t2ut7bCQNuEj8DDES6BZk
          --dependency=base=base-4.12.0.0
          --dependency=bytestring=bytestring-0.10.8.2
          --dependency=containers=containers-0.6.0.1
          --dependency=gi-glib=gi-glib-2.0.17-3yKSiUQ3LF9JT4nRrmtYs7
          --dependency=gi-gobject=gi-gobject-2.0.19-3Dy0Jy2lzhs7ZIY2O6xEjB
          --dependency=haskell-gi=haskell-gi-0.21.5-3m43gSnvNGIlAqKEDTlmk
          --dependency=haskell-gi-base=haskell-gi-base-0.21.5-B7L3HQFSPkOFeLcsLCK63O
          --dependency=haskell-gi-overloading=haskell-gi-overloading-1.0-6w3IXGDooXgu3RA2LfTuf
          --dependency=text=text-1.2.3.1
          --dependency=transformers=transformers-0.5.6.2
          --exact-configuration
          --ghc-option=-fhide-source-paths
    Process exited with code: ExitFailure 1
Progress 1/6
```

# 調査

## 今わかっていること

- `gi-gtk-declarative`及び`gi-gtk-declarative-app-simple`を`package.yaml`に追加するまではビルドできていた
- `gi-gtk-declarative-app-simple`をなくしても同様のエラーが出るため、問題があるとすれば`gi-gtk-declarative`関連
- [haskell-gi/haskell-gi #239](https://github.com/haskell-gi/haskell-gi/issues/239)は確認しているが、`GObject`のバージョンは新しいためおそらくこれは関係がない
- [owickstrom/gi-gtk-declarative](https://github.com/owickstrom/gi-gtk-declarative)のexampleはビルドできている。該当コミットは[4a04e4c](https://github.com/owickstrom/gi-gtk-declarative/tree/4a04e4c748a36856f68bae26d68806303a08af3f)

## 1. 公式のexampleをビルドしてみる

`gi-gtk-declarative`の問題ならexampleもビルドできないはずなので、まずはexampleのビルドをしてみる。
[BUILD.md](https://github.com/owickstrom/gi-gtk-declarative/blob/4a04e4c748a36856f68bae26d68806303a08af3f/BUILD.md)より、`stack build`でライブラリをビルドできるようなので試す


```sh
<X_X>:gi-gtk-declarative$ stack clean --full
(GHQ) $?is0[master]<ttys014>
<X_X>:gi-gtk-declarative$ stack build
gi-gtk                       > configure
gi-gtk                       > [1 of 2] Compiling Main             ( /private/var/folders/f4/_n6jxvsd1wd5cck49ntypshc0000gn/T/stack-dc4700af7a03ac2d/gi-gtk-3.0.27/Setup.hs, /private/var/folders/f4/_n6jxvsd1wd5cck49ntypshc0000gn/T/stack-dc4700af7a03ac2d/gi-gtk-3.0.27/.stack-work/dist/x86_64-osx/Cabal-2.4.0.1/setup/Main.o )
gi-gtk                       > [2 of 2] Compiling StackSetupShim   ( /Users/me/.ghq/github.com/Cj-bc/dotfiles/dotfiles/stack/setup-exe-src/setup-shim-mPHDZzAJ.hs, /private/var/folders/f4/_n6jxvsd1wd5cck49ntypshc0000gn/T/stack-dc4700af7a03ac2d/gi-gtk-3.0.27/.stack-work/dist/x86_64-osx/Cabal-2.4.0.1/setup/StackSetupShim.o )
gi-gtk                       > Linking /private/var/folders/f4/_n6jxvsd1wd5cck49ntypshc0000gn/T/stack-dc4700af7a03ac2d/gi-gtk-3.0.27/.stack-work/dist/x86_64-osx/Cabal-2.4.0.1/setup/setup ...
gi-gtk                       > Parse error: Error when parsing "record [_MountOperationHandlerProxy] / field [priv] / type": Expected attribute "Name {nameLocalName = "name", nameNamespace = Nothing, namePrefix = Nothing}" not present.
gi-gtk                       > Element
gi-gtk                       >   { elementName =
gi-gtk                       >       Name
gi-gtk                       >         { nameLocalName = "type"
gi-gtk                       >         , nameNamespace = Just "http://www.gtk.org/introspection/core/1.0"
gi-gtk                       >         , namePrefix = Nothing
gi-gtk                       >         }
gi-gtk                       >   , elementAttributes =
gi-gtk                       >       fromList
gi-gtk                       >         [ ( Name
gi-gtk                       >               { nameLocalName = "type"
gi-gtk                       >               , nameNamespace = Just "http://www.gtk.org/introspection/c/1.0"
gi-gtk                       >               , namePrefix = Just "c"
gi-gtk                       >               }
gi-gtk                       >           , "_GtkMountOperationHandlerProxyPrivate*"
gi-gtk                       >           )
gi-gtk                       >         ]
gi-gtk                       >   , elementNodes = []
gi-gtk                       >   }
gi-gtk                       > CallStack (from HasCallStack):
gi-gtk                       >   error, called at lib/Data/GI/CodeGen/API.hs:175:19 in haskell-gi-0.21.5-6CqMZo5dCtzICRoQotPRBa:Data.GI.CodeGen.API
            
--  While building package gi-gtk-3.0.27 using:
      /private/var/folders/f4/_n6jxvsd1wd5cck49ntypshc0000gn/T/stack-dc4700af7a03ac2d/gi-gtk-3.0.27/.stack-work/dist/x86_64-osx/Cabal-2.4.0.1/setup/setup 
          --builddir=.stack-work/dist/x86_64-osx/Cabal-2.4.0.1 configure 
          --user 
          --package-db=clear 
          --package-db=global 
          --package-db=/Users/me/.ghq/github.com/Cj-bc/dotfiles/dotfiles/stack/snapshots/x86_64-osx/f0802d4dcb3bb60227896a26d54489c96a0c759323d166e1c1a8931886f052f1/8.6.5/pkgdb 
          --libdir=/Users/me/.ghq/github.com/Cj-bc/dotfiles/dotfiles/stack/snapshots/x86_64-osx/f0802d4dcb3bb60227896a26d54489c96a0c759323d166e1c1a8931886f052f1/8.6.5/lib 
          --bindir=/Users/me/.ghq/github.com/Cj-bc/dotfiles/dotfiles/stack/snapshots/x86_64-osx/f0802d4dcb3bb60227896a26d54489c96a0c759323d166e1c1a8931886f052f1/8.6.5/bin 
          --datadir=/Users/me/.ghq/github.com/Cj-bc/dotfiles/dotfiles/stack/snapshots/x86_64-osx/f0802d4dcb3bb60227896a26d54489c96a0c759323d166e1c1a8931886f052f1/8.6.5/share 
          --libexecdir=/Users/me/.ghq/github.com/Cj-bc/dotfiles/dotfiles/stack/snapshots/x86_64-osx/f0802d4dcb3bb60227896a26d54489c96a0c759323d166e1c1a8931886f052f1/8.6.5/libexec 
          --sysconfdir=/Users/me/.ghq/github.com/Cj-bc/dotfiles/dotfiles/stack/snapshots/x86_64-osx/f0802d4dcb3bb60227896a26d54489c96a0c759323d166e1c1a8931886f052f1/8.6.5/etc 
          --docdir=/Users/me/.ghq/github.com/Cj-bc/dotfiles/dotfiles/stack/snapshots/x86_64-osx/f0802d4dcb3bb60227896a26d54489c96a0c759323d166e1c1a8931886f052f1/8.6.5/doc/gi-gtk-3.0.27 
          --htmldir=/Users/me/.ghq/github.com/Cj-bc/dotfiles/dotfiles/stack/snapshots/x86_64-osx/f0802d4dcb3bb60227896a26d54489c96a0c759323d166e1c1a8931886f052f1/8.6.5/doc/gi-gtk-3.0.27 
          --haddockdir=/Users/me/.ghq/github.com/Cj-bc/dotfiles/dotfiles/stack/snapshots/x86_64-osx/f0802d4dcb3bb60227896a26d54489c96a0c759323d166e1c1a8931886f052f1/8.6.5/doc/gi-gtk-3.0.27 
          --dependency=Cabal=Cabal-2.4.1.0-4t2ut7bCQNuEj8DDES6BZk 
          --dependency=base=base-4.12.0.0 
          --dependency=bytestring=bytestring-0.10.8.2 
          --dependency=containers=containers-0.6.0.1 
          --dependency=gi-atk=gi-atk-2.0.15-H5Jq1m2kNH65jfpygEJgbD 
          --dependency=gi-cairo=gi-cairo-1.0.17-5J4N4q9u6Ut6LsSL0RLl3y 
          --dependency=gi-gdk=gi-gdk-3.0.16-7fp6wkQtYQB2OFt93pmoGS 
          --dependency=gi-gdkpixbuf=gi-gdkpixbuf-2.0.18-ILYayPpcFcIIlRmH6MXr6k 
          --dependency=gi-gio=gi-gio-2.0.19-BYWnoq9VR9YIfYgwPLuJAa 
          --dependency=gi-glib=gi-glib-2.0.17-5oAqeCxHj4GLcxEBFLI3NM 
          --dependency=gi-gobject=gi-gobject-2.0.16-5WDst44cUXzHAJ7zlDBwwB 
          --dependency=gi-pango=gi-pango-1.0.16-75tBNmJ6cGpL1K8bM8cclg 
          --dependency=haskell-gi=haskell-gi-0.21.5-6CqMZo5dCtzICRoQotPRBa 
          --dependency=haskell-gi-base=haskell-gi-base-0.21.5-B7L3HQFSPkOFeLcsLCK63O 
          --dependency=haskell-gi-overloading=haskell-gi-overloading-1.0-6w3IXGDooXgu3RA2LfTuf 
          --dependency=text=text-1.2.3.1 
          --dependency=transformers=transformers-0.5.6.2 
          --exact-configuration 
          --ghc-option=-fhide-source-paths
    Process exited with code: ExitFailure 1
```

:thinking:

exampleについては別途記載があるので`cabal`を試してみる

```
<X_X>:examples$ cabal new-clean
(GHQ) $?is0<ttys014>
<X_X>:examples$ cabal new-run example Hello
Resolving dependencies...
Build profile: -w ghc-8.8.1 -O1
In order, the following will be built (use -v for more details):
 - gi-gtk-declarative-0.5.0 (lib) (first run)
 - gi-gtk-declarative-app-simple-0.5.0 (lib) (first run)
 - examples-0.5.0 (exe:example) (first run)
Configuring library for gi-gtk-declarative-0.5.0..
Preprocessing library for gi-gtk-declarative-0.5.0..
Building library for gi-gtk-declarative-0.5.0..
[ 1 of 21] Compiling GI.Gtk.Declarative.Attributes.Internal.EventHandler ( src/GI/Gtk/Declarative/Attributes/Internal/EventHandler.hs, /Users/me/.ghq/github.com/owickstrom/gi-gtk-declarative/dist-newstyle/build/x86_64-osx/ghc-8.8.1/gi-gtk-declarative-0.5.0/build/GI/Gtk/Declarative/Attributes/Internal/EventHandler.o )
[ 2 of 21] Compiling GI.Gtk.Declarative.Attributes.Internal.Conversions ( src/GI/Gtk/Declarative/Attributes/Internal/Conversions.hs, /Users/me/.ghq/github.com/owickstrom/gi-gtk-declarative/dist-newstyle/build/x86_64-osx/ghc-8.8.1/gi-gtk-declarative-0.5.0/build/GI/Gtk/Declarative/Attributes/Internal/Conversions.o )
[ 3 of 21] Compiling GI.Gtk.Declarative.Attributes ( src/GI/Gtk/Declarative/Attributes.hs, /Users/me/.ghq/github.com/owickstrom/gi-gtk-declarative/dist-newstyle/build/x86_64-osx/ghc-8.8.1/gi-gtk-declarative-0.5.0/build/GI/Gtk/Declarative/Attributes.o )
[ 4 of 21] Compiling GI.Gtk.Declarative.Attributes.Collected ( src/GI/Gtk/Declarative/Attributes/Collected.hs, /Users/me/.ghq/github.com/owickstrom/gi-gtk-declarative/dist-newstyle/build/x86_64-osx/ghc-8.8.1/gi-gtk-declarative-0.5.0/build/GI/Gtk/Declarative/Attributes/Collected.o )
[ 5 of 21] Compiling GI.Gtk.Declarative.Container.Class ( src/GI/Gtk/Declarative/Container/Class.hs, /Users/me/.ghq/github.com/owickstrom/gi-gtk-declarative/dist-newstyle/build/x86_64-osx/ghc-8.8.1/gi-gtk-declarative-0.5.0/build/GI/Gtk/Declarative/Container/Class.o )
[ 6 of 21] Compiling GI.Gtk.Declarative.State ( src/GI/Gtk/Declarative/State.hs, /Users/me/.ghq/github.com/owickstrom/gi-gtk-declarative/dist-newstyle/build/x86_64-osx/ghc-8.8.1/gi-gtk-declarative-0.5.0/build/GI/Gtk/Declarative/State.o )
[ 7 of 21] Compiling GI.Gtk.Declarative.Patch ( src/GI/Gtk/Declarative/Patch.hs, /Users/me/.ghq/github.com/owickstrom/gi-gtk-declarative/dist-newstyle/build/x86_64-osx/ghc-8.8.1/gi-gtk-declarative-0.5.0/build/GI/Gtk/Declarative/Patch.o )
[ 8 of 21] Compiling GI.Gtk.Declarative.EventSource ( src/GI/Gtk/Declarative/EventSource.hs, /Users/me/.ghq/github.com/owickstrom/gi-gtk-declarative/dist-newstyle/build/x86_64-osx/ghc-8.8.1/gi-gtk-declarative-0.5.0/build/GI/Gtk/Declarative/EventSource.o )
[ 9 of 21] Compiling GI.Gtk.Declarative.Attributes.Internal ( src/GI/Gtk/Declarative/Attributes/Internal.hs, /Users/me/.ghq/github.com/owickstrom/gi-gtk-declarative/dist-newstyle/build/x86_64-osx/ghc-8.8.1/gi-gtk-declarative-0.5.0/build/GI/Gtk/Declarative/Attributes/Internal.o )
[10 of 21] Compiling GI.Gtk.Declarative.CustomWidget ( src/GI/Gtk/Declarative/CustomWidget.hs, /Users/me/.ghq/github.com/owickstrom/gi-gtk-declarative/dist-newstyle/build/x86_64-osx/ghc-8.8.1/gi-gtk-declarative-0.5.0/build/GI/Gtk/Declarative/CustomWidget.o )
[11 of 21] Compiling GI.Gtk.Declarative.Container.Patch ( src/GI/Gtk/Declarative/Container/Patch.hs, /Users/me/.ghq/github.com/owickstrom/gi-gtk-declarative/dist-newstyle/build/x86_64-osx/ghc-8.8.1/gi-gtk-declarative-0.5.0/build/GI/Gtk/Declarative/Container/Patch.o )

src/GI/Gtk/Declarative/Container/Patch.hs:22:1: warning: [-Wunused-imports]
    The import of ‘Data.Foldable’ is redundant
      except perhaps to import instances from ‘Data.Foldable’
    To import instances alone, use: import Data.Foldable()
   |
22 | import           Data.Foldable                      (foldMap)
   | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
[12 of 21] Compiling GI.Gtk.Declarative.Widget ( src/GI/Gtk/Declarative/Widget.hs, /Users/me/.ghq/github.com/owickstrom/gi-gtk-declarative/dist-newstyle/build/x86_64-osx/ghc-8.8.1/gi-gtk-declarative-0.5.0/build/GI/Gtk/Declarative/Widget.o )
[13 of 21] Compiling GI.Gtk.Declarative.SingleWidget ( src/GI/Gtk/Declarative/SingleWidget.hs, /Users/me/.ghq/github.com/owickstrom/gi-gtk-declarative/dist-newstyle/build/x86_64-osx/ghc-8.8.1/gi-gtk-declarative-0.5.0/build/GI/Gtk/Declarative/SingleWidget.o )
[14 of 21] Compiling GI.Gtk.Declarative.Container.Box ( src/GI/Gtk/Declarative/Container/Box.hs, /Users/me/.ghq/github.com/owickstrom/gi-gtk-declarative/dist-newstyle/build/x86_64-osx/ghc-8.8.1/gi-gtk-declarative-0.5.0/build/GI/Gtk/Declarative/Container/Box.o )
[15 of 21] Compiling GI.Gtk.Declarative.Container ( src/GI/Gtk/Declarative/Container.hs, /Users/me/.ghq/github.com/owickstrom/gi-gtk-declarative/dist-newstyle/build/x86_64-osx/ghc-8.8.1/gi-gtk-declarative-0.5.0/build/GI/Gtk/Declarative/Container.o )
[16 of 21] Compiling GI.Gtk.Declarative.Container.Paned ( src/GI/Gtk/Declarative/Container/Paned.hs, /Users/me/.ghq/github.com/owickstrom/gi-gtk-declarative/dist-newstyle/build/x86_64-osx/ghc-8.8.1/gi-gtk-declarative-0.5.0/build/GI/Gtk/Declarative/Container/Paned.o )
[17 of 21] Compiling GI.Gtk.Declarative.Bin ( src/GI/Gtk/Declarative/Bin.hs, /Users/me/.ghq/github.com/owickstrom/gi-gtk-declarative/dist-newstyle/build/x86_64-osx/ghc-8.8.1/gi-gtk-declarative-0.5.0/build/GI/Gtk/Declarative/Bin.o )
[18 of 21] Compiling GI.Gtk.Declarative.Container.MenuItem ( src/GI/Gtk/Declarative/Container/MenuItem.hs, /Users/me/.ghq/github.com/owickstrom/gi-gtk-declarative/dist-newstyle/build/x86_64-osx/ghc-8.8.1/gi-gtk-declarative-0.5.0/build/GI/Gtk/Declarative/Container/MenuItem.o )
[19 of 21] Compiling GI.Gtk.Declarative.Container.ListBox ( src/GI/Gtk/Declarative/Container/ListBox.hs, /Users/me/.ghq/github.com/owickstrom/gi-gtk-declarative/dist-newstyle/build/x86_64-osx/ghc-8.8.1/gi-gtk-declarative-0.5.0/build/GI/Gtk/Declarative/Container/ListBox.o )
[20 of 21] Compiling GI.Gtk.Declarative.Widget.Conversions ( src/GI/Gtk/Declarative/Widget/Conversions.hs, /Users/me/.ghq/github.com/owickstrom/gi-gtk-declarative/dist-newstyle/build/x86_64-osx/ghc-8.8.1/gi-gtk-declarative-0.5.0/build/GI/Gtk/Declarative/Widget/Conversions.o )
[21 of 21] Compiling GI.Gtk.Declarative ( src/GI/Gtk/Declarative.hs, /Users/me/.ghq/github.com/owickstrom/gi-gtk-declarative/dist-newstyle/build/x86_64-osx/ghc-8.8.1/gi-gtk-declarative-0.5.0/build/GI/Gtk/Declarative.o )
Configuring library for gi-gtk-declarative-app-simple-0.5.0..
Preprocessing library for gi-gtk-declarative-app-simple-0.5.0..
Building library for gi-gtk-declarative-app-simple-0.5.0..
[1 of 1] Compiling GI.Gtk.Declarative.App.Simple ( src/GI/Gtk/Declarative/App/Simple.hs, /Users/me/.ghq/github.com/owickstrom/gi-gtk-declarative/dist-newstyle/build/x86_64-osx/ghc-8.8.1/gi-gtk-declarative-app-simple-0.5.0/build/GI/Gtk/Declarative/App/Simple.o )

src/GI/Gtk/Declarative/App/Simple.hs:73:6: warning: [-Wsimplifiable-class-constraints]
    • The constraint ‘Gtk.IsBin window’ matches
        instance (Gtk.GObject o,
                  Data.GI.Base.Overloading.IsDescendantOf Gtk.Bin o) =>
                 Gtk.IsBin o
          -- Defined in ‘GI.Gtk.Objects.Bin’
      This makes type inference for inner bindings fragile;
        either use MonoLocalBinds, or simplify it using the instance
    • In the type signature:
        run :: Gtk.IsBin window => App window state event -> IO state
   |
73 |   :: Gtk.IsBin window
   |      ^^^^^^^^^^^^^^^^...

src/GI/Gtk/Declarative/App/Simple.hs:99:6: warning: [-Wsimplifiable-class-constraints]
    • The constraint ‘Gtk.IsBin window’ matches
        instance (Gtk.GObject o,
                  Data.GI.Base.Overloading.IsDescendantOf Gtk.Bin o) =>
                 Gtk.IsBin o
          -- Defined in ‘GI.Gtk.Objects.Bin’
      This makes type inference for inner bindings fragile;
        either use MonoLocalBinds, or simplify it using the instance
    • In the type signature:
        runLoop :: Gtk.IsBin window => App window state event -> IO state
   |
99 |   :: Gtk.IsBin window
   |      ^^^^^^^^^^^^^^^^...
Configuring executable 'example' for examples-0.5.0..
Preprocessing executable 'example' for examples-0.5.0..
Building executable 'example' for examples-0.5.0..
[ 1 of 13] Compiling AddBoxes         ( AddBoxes.hs, /Users/me/.ghq/github.com/owickstrom/gi-gtk-declarative/dist-newstyle/build/x86_64-osx/ghc-8.8.1/examples-0.5.0/x/example/build/example/example-tmp/AddBoxes.o )
[ 2 of 13] Compiling CSS              ( CSS.hs, /Users/me/.ghq/github.com/owickstrom/gi-gtk-declarative/dist-newstyle/build/x86_64-osx/ghc-8.8.1/examples-0.5.0/x/example/build/example/example-tmp/CSS.o )
[ 3 of 13] Compiling CustomWidget     ( CustomWidget.hs, /Users/me/.ghq/github.com/owickstrom/gi-gtk-declarative/dist-newstyle/build/x86_64-osx/ghc-8.8.1/examples-0.5.0/x/example/build/example/example-tmp/CustomWidget.o )
[ 4 of 13] Compiling Dialog           ( Dialog.hs, /Users/me/.ghq/github.com/owickstrom/gi-gtk-declarative/dist-newstyle/build/x86_64-osx/ghc-8.8.1/examples-0.5.0/x/example/build/example/example-tmp/Dialog.o )
[ 5 of 13] Compiling Exit             ( Exit.hs, /Users/me/.ghq/github.com/owickstrom/gi-gtk-declarative/dist-newstyle/build/x86_64-osx/ghc-8.8.1/examples-0.5.0/x/example/build/example/example-tmp/Exit.o )
[ 6 of 13] Compiling FileChooserButton ( FileChooserButton.hs, /Users/me/.ghq/github.com/owickstrom/gi-gtk-declarative/dist-newstyle/build/x86_64-osx/ghc-8.8.1/examples-0.5.0/x/example/build/example/example-tmp/FileChooserButton.o )
[ 7 of 13] Compiling Functor          ( Functor.hs, /Users/me/.ghq/github.com/owickstrom/gi-gtk-declarative/dist-newstyle/build/x86_64-osx/ghc-8.8.1/examples-0.5.0/x/example/build/example/example-tmp/Functor.o )
[ 8 of 13] Compiling Hello            ( Hello.hs, /Users/me/.ghq/github.com/owickstrom/gi-gtk-declarative/dist-newstyle/build/x86_64-osx/ghc-8.8.1/examples-0.5.0/x/example/build/example/example-tmp/Hello.o )

Hello.hs:11:1: warning: [-Wunused-imports]
    The import of ‘Control.Monad’ is redundant
      except perhaps to import instances from ‘Control.Monad’
    To import instances alone, use: import Control.Monad()
   |
11 | import           Control.Monad                 (void)
   | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
[ 9 of 13] Compiling ListBox          ( ListBox.hs, /Users/me/.ghq/github.com/owickstrom/gi-gtk-declarative/dist-newstyle/build/x86_64-osx/ghc-8.8.1/examples-0.5.0/x/example/build/example/example-tmp/ListBox.o )

ListBox.hs:8:1: warning: [-Wunused-imports]
    The import of ‘Control.Monad’ is redundant
      except perhaps to import instances from ‘Control.Monad’
    To import instances alone, use: import Control.Monad()
  |
8 | import           Control.Monad                 (void)
  | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
[10 of 13] Compiling ManyBoxes        ( ManyBoxes.hs, /Users/me/.ghq/github.com/owickstrom/gi-gtk-declarative/dist-newstyle/build/x86_64-osx/ghc-8.8.1/examples-0.5.0/x/example/build/example/example-tmp/ManyBoxes.o )
[11 of 13] Compiling MenuBar          ( MenuBar.hs, /Users/me/.ghq/github.com/owickstrom/gi-gtk-declarative/dist-newstyle/build/x86_64-osx/ghc-8.8.1/examples-0.5.0/x/example/build/example/example-tmp/MenuBar.o )
[12 of 13] Compiling Paned            ( Paned.hs, /Users/me/.ghq/github.com/owickstrom/gi-gtk-declarative/dist-newstyle/build/x86_64-osx/ghc-8.8.1/examples-0.5.0/x/example/build/example/example-tmp/Paned.o )
[13 of 13] Compiling Main             ( Main.hs, /Users/me/.ghq/github.com/owickstrom/gi-gtk-declarative/dist-newstyle/build/x86_64-osx/ghc-8.8.1/examples-0.5.0/x/example/build/example/example-tmp/Main.o )
Linking /Users/me/.ghq/github.com/owickstrom/gi-gtk-declarative/dist-newstyle/build/x86_64-osx/ghc-8.8.1/examples-0.5.0/x/example/build/example/example ...
```

色々とWarningはあるものの動いた。


## 2. cabalを直接使ってビルドしてみる
