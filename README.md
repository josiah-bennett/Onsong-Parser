<div id="top"></div>
<!--
*** Thanks for checking out the Best-README-Template. If you have a suggestion
*** that would make this better, please fork the repo and create a pull request
*** or simply open an issue with the tag "enhancement".
*** Don't forget to give the project a star!
*** Thanks again! Now go create something AMAZING! :D
-->



<!-- PROJECT SHIELDS -->
<!--
*** I'm using markdown "reference style" links for readability.
*** Reference links are enclosed in brackets [ ] instead of parentheses ( ).
*** See the bottom of this document for the declaration of the reference variables
*** for contributors-url, forks-url, etc. This is an optional, concise syntax you may use.
*** https://www.markdownguide.org/basic-syntax/#reference-style-links
-->
<!--[![Contributors][contributors-shield]][contributors-url]-->
<!--[![Forks][forks-shield]][forks-url]-->
<!--[![Stargazers][stars-shield]][stars-url]-->
[![Issues][issues-shield]][issues-url]
[![GPL v3.0 License][license-shield]][license-url]



<!-- PROJECT LOGO -->
<br />
<div align="center">
  <!--<a href="https://github.com/josiah-bennett/Onsong-Parser">
    <img src="images/logo.png" alt="Logo" width="80" height="80">
  </a>-->

<h3 align="center">Onsong to Html Parser</h3>

  <p align="center">
    program that reads and parses .onsong and potentially .chopro files to a html format.
    <br />
    <a href="https://github.com/josiah-bennett/Onsong-Parser"><strong>Explore the docs »</strong></a>
    <br />
    <br />
    <a href="https://github.com/josiah-bennett/Onsong-Parser">View Demo</a>
    ·
    <a href="https://github.com/josiah-bennett/Onsong-Parser/issues">Report Bug</a>
    ·
    <a href="https://github.com/josiah-bennett/Onsong-Parser/issues">Request Feature</a>
  </p>
</div>



<!-- TABLE OF CONTENTS -->
<details>
  <summary>Table of Contents</summary>
  <ol>
    <li>
      <a href="#about-the-project">About The Project</a>
    </li>
    <li>
      <a href="#getting-started">Getting Started</a>
      <ul>
        <li><a href="#prerequisites">Prerequisites</a></li>
        <li><a href="#installation">Installation</a></li>
      </ul>
    </li>
    <li><a href="#usage">Usage</a></li>
    <li><a href="#roadmap">Roadmap</a></li>
    <li><a href="#contributing">Contributing</a></li>
    <li><a href="#license">License</a></li>
    <li><a href="#contact">Contact</a></li>
    <li><a href="#acknowledgments">Acknowledgments</a></li>
  </ol>
</details>



<!-- ABOUT THE PROJECT -->
## About The Project

<!--[![Product Name Screen Shot][product-screenshot]](https://example.com)-->

A parser for .onsong (and potentially .chopro) files to create html files, made in Haskell.
Written for my static site generator (repo doesn't exist yet) to create an interface for a large onsong-library I wanted to host on a raspberry pi.
It was made because I wanted to share my songs with people for a gathering without everyone owning the Onsong App and not being able to project the lyrics onto a screen for everyone.

<p align="right">(<a href="#top">back to top</a>)</p>



<!-- GETTING STARTED -->
## Getting Started

To get a local copy up and running follow the steps outlined below.



### Installation

#### With stack

Install stack either with your package manager:
```sh
sudo apt install haskell-stack  # Debian, Ubuntu
sudo dnf install stack 		# Fedora
sudo pacman -S stack		# Arch
```

And don't forget to upgrade stack as your package manager will probably not
have the most recent version installed
```sh
stack upgrade
```

Or install it via the recommended way [Documentation](https://docs.haskellstack.org/en/stable/README/):
```sh
curl -sSL https://get.haskellstack.org/ | sh
```

Then follow these simple steps to get a development version running:

1. Clone the repo
   ```sh
   git clone https://github.com/josiah-bennett/Onsong-Parser.git
   ```
2. Initialize project
   ```sh
   stack init
   ```
3. Build project
   ```sh
   stack build
   ```

If you want to have an executable, run `stack install` instead of `stack build` and make sure that `$HOME/.local/bin` is in your $PATH

#### With cabal-install

TODO right here ...

<p align="right">(<a href="#top">back to top</a>)</p>



<!-- USAGE EXAMPLES -->
## Usage

Currently just run the compiled program in the root directory.
NOTE: make sure the filename specified is specified without a fileending the progam will handle that automatically.
```
e.g. for a file `song.onsong` use `song` as the filename
```

<!--_For more examples, please refer to the [Documentation](https://example.com)_-->

<p align="right">(<a href="#top">back to top</a>)</p>



<!-- ROADMAP -->
## Roadmap

- [x] support for multiple empty lines between 'paragraphs'
- [ ] metadata parsing
- [ ] add proper ChordPro support
- [ ] add proper interactability and error handling
- [ ] implement a proper testing suite

See the [open issues](https://github.com/josiah-bennett/Onsong-Parser/issues) for a full list of proposed features (and known issues).

<p align="right">(<a href="#top">back to top</a>)</p>



<!-- CONTRIBUTING -->
## Contributing

Contributions are what make the open source community such an amazing place to learn, inspire, and create. Any contributions you make are **greatly appreciated**.

If you have a suggestion that would make this better, please fork the repo and create a pull request. You can also simply open an issue with the tag "enhancement".
Don't forget to give the project a star! Thanks again!

1. Fork the Project
2. Create your Feature Branch (`git checkout -b feature/AmazingFeature`)
3. Commit your Changes (`git commit -m 'Add some AmazingFeature'`)
4. Push to the Branch (`git push origin feature/AmazingFeature`)
5. Open a Pull Request

<p align="right">(<a href="#top">back to top</a>)</p>



<!-- LICENSE -->
## License

Distributed under the GPL v3.0 License. See `LICENSE.md` for more information.

<p align="right">(<a href="#top">back to top</a>)</p>



<!-- CONTACT -->
## Contact

Josiah - josiah.bennett@web.de

Project Link: [https://github.com/josiah-bennett/Onsong-Parser](https://github.com/josiah-bennett/Onsong-Parser)

<p align="right">(<a href="#top">back to top</a>)</p>



<!-- ACKNOWLEDGMENTS -->
## Acknowledgments

* [README Template](https://github.com/othneildrew/Best-README-Template)
<!--* []()-->
<!--* []()-->

<p align="right">(<a href="#top">back to top</a>)</p>



<!-- MARKDOWN LINKS & IMAGES -->
<!-- https://www.markdownguide.org/basic-syntax/#reference-style-links -->
[contributors-shield]: https://img.shields.io/github/contributors/josiah-bennett/Onsong-Parser.svg?style=for-the-badge
[contributors-url]: https://github.com/josiah-bennett/Onsong-Parser/graphs/contributors
[forks-shield]: https://img.shields.io/github/forks/josiah-bennett/Onsong-Parser.svg?style=for-the-badge
[forks-url]: https://github.com/josiah-bennett/Onsong-Parser/network/members
[stars-shield]: https://img.shields.io/github/stars/josiah-bennett/Onsong-Parser.svg?style=for-the-badge
[stars-url]: https://github.com/josiah-bennett/Onsong-Parser/stargazers
[issues-shield]: https://img.shields.io/github/issues/josiah-bennett/Onsong-Parser.svg?style=for-the-badge
[issues-url]: https://github.com/josiah-bennett/Onsong-Parser/issues
[license-shield]: https://img.shields.io/github/license/josiah-bennett/Onsong-Parser.svg?style=for-the-badge
[license-url]: https://github.com/josiah-bennett/Onsong-Parser/blob/master/LICENSE.md
[product-screenshot]: images/screenshot.png

