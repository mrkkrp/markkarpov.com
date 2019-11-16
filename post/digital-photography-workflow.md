---
title: Digital photography workflow for Linux
desc: Digital photgraphy workflow I use on Linux.
date:
  published: November 16, 2019
tag: photography
---

```toc
```

A few weeks ago I started with photography as a hobbyist. As a software
engineer, one of the first things I wanted to get right is the process I'm
going to use for developing my photos and storing the raw files. One
constraint is that I use Linux and I'm not willing to touch Windows or buy a
Mac, so this post is going to target people like me—Linux users who are
technically-inclined enough to use something like version control system to
organize their photos.

## RAW files

Set your camera to produce RAW files. RAW files contain the “raw” data from
the sensor, and this allows you to achieve better results in
post-production. JPEG can be always obtained from RAW, so there is no good
reason to shoot in JPEG alone or combined with RAW.

The problem with RAW though, is that it's a term which does not map to a
single file format. In fact, almost every camera has its own RAW format. For
example, my Canon EOS R writes files with `CR3` extension which is different
from `CR2` older Canon cameras produced and is also different from the
formats other camera manufactures use.

It's not a big deal for users of proprietary software like [Abobe
Lightroom][lightroom] on Windows or Mac because Canon promptly gives those
players SDKs which allows them to work with the new formats. The situation
is different for Linux users. We have to reverse-engineer the format. It
takes time. In fact, it's been 1 year since release of Canon EOS R and it's
still not supported by popular RAW developer applications such as
[Darktable][darktable].

Happily, there is a solution. Adobe created a unified RAW format with open
specification which is implemented in all decent Linux software by now. The
format is called [Digital Negative (DNG)][dng].

I think that in general, keeping all RAW data in DNG is a good idea. Since
it's an open and widely supported format, it enables you to do more with
different tools, including the tools you don't know yet, so it is more
future-proof.

## Converting to DNG

Now the question is: how to convert your RAW files to DNG? The best way to
do it is with Adobe's DNG converter. The program only works on Windows and
Mac though, so we'll have to use Wine.

Here are the step-by-step instructions:

1. Download the executable for Windows from the [official
   site][dng-downloads]. Make sure to use the latest version because it may
   have more accurate color matrices and other improvements. This is also
   the reason why using this converter from Adobe is preferable to using an
   open source equivalent—even if the open source converter supports the RAW
   format specific to your camera, it'll probably have less accurate color
   matrices and hence will produce less accurate results. You do not want
   that.

2. Install [Wine][wine] from the repositories of your distribution. Make
   sure to do whatever is necessary to make Wine support both 32-bit and
   64-bit executables, otherwise you'll run into problems, e.g. the
   executable files may not appear after installation.

   On [NixOS][nixos] I found that `wine.override { wineBuild = "wineWow"; }`
   instead of simple `wine` works. I found the trick [here][nixos-wine].

3. Run the installer via Wine (at the time of this writing version 12.0 is
   the latest):

   ```
   $ wine DNGConverter_12_0.exe
   ```

   Click `Install`, there should be no problems with this.

4. Check if the installer created `Adobe DNG Converter.exe`. I found it in
   `~/.wine/drive_c/Program Files/Adobe/Adobe DNG Converter/`.

5. Run the file using Wine:

   ```
   $ wine "Adobe DNG Converter.exe"
   ```

   [It will probably fail][wine-bug] though. If it is indeed the case follow
   step 6, otherwise you can skip it.

6. Run `winecfg`. Go to the `Libraries` tab. Add a new override for
   `api-ms-win-core-winrt-error-l1-1-0`, then edit it and select `Disable`.

7. Convert your files to the DNG format.

## Developing the photos

I use [Darktable][darktable]. It follows the “non-destructive” editing
model, meaning that your RAW files stay intact. Instead it creates files in
the XMP format that describe the operations you apply to the RAW files. Then
you can export the results as e.g. JPEG images.

## Storing the photos

The only thing that changes is the XMP files and those can be stored in
version control system such as [Git][git]. Git is something most software
developers use to keep track of changes in source code. Since I'm a
developer, it was natural for me to see that I can store the XMP files of
Darktable in a Git repository. This way I can use [GitHub][github] as my
backup and I can go back and forth in history of edits so that nothing can
be lost.

What about the RAW files? The problem is that those are relatively big
multimedia files, and Git was designed to work with text files such as
source code of programs, not with multimedia files. Fortunately, there is a
Git extension called [Git Large file storage (LFS)][git-lfs] which improves
user experience when the repository contains big files.

What about the JPEG files? The resulting rendered files can always be
obtained from the combination of RAW files and corresponding Darktable XMP
files. Thus, it's not necessary to store the JPEG files in the Git
repository.

So here is how to set things up:

1. Install `git` and `git-lfs` from the repositories of your distribution.

2. Register on [GitHub][github]. Create a repository there. Now GitHub
   provides unlimited private repositories for free.

3. Clone the repository:

   ```
   $ git clone git@github.com:user-name/repo-name.git destination-folder
   ```

   This will require first setting your SSH key on GitHub (I will not cover
   it here).

4. Copy your files in the repository.

5. Create `.gitignore` file which will disable tracking of the rendered
   files. This means that it could contain, e.g. something like

   ```
   *.jpg
   *.jpeg
   ```

6. Tell Git to treat your DNG files specially by using `git-lfs`:

   ```
   $ git lfs track "*.dng"
   ```

   this will create a `gitattributes` file.

7. Add all the files, commit them, and push to GitHub:

   ```
   $ git add -A
   $ git commit
   $ git push origin master
   ```

   Getting familiar with Git will take some time but it's really worth it if
   you need to deal with data that changes over time. You can also keep all
   sorts of files and notes together with your photos this way.

8. GitHub [says][github-lfs-limits] it allows us to use 1Gb of storage for
   free and then you can pay 5$ per month for 50 Gb. Which is OK if you ask
   me.

## Conclusion

Here is the approach I've come up with. When I need to show my photos to the
world I just render them using Darktable and upload wherever is necessary. I
can always access all my RAW files and Darktable tweaks. I can also go back
in history so no version of a photo is ever lost, even if I edit it.

GitHub works as a reliable backup in the cloud. Even if my computer gets
lost or breaks restoring my data is as easy as cloning the repository again
and re-rendering the JPEG files.

[darktable]: https://www.darktable.org
[lightroom]: https://www.adobe.com/products/photoshop-lightroom.html
[dng]: https://helpx.adobe.com/photoshop/digital-negative.html
[dng-downloads]: https://helpx.adobe.com/photoshop/digital-negative.html#downloads
[wine]: https://www.winehq.org
[nixos]: https://nixos.org
[nixos-wine]: https://nixos.wiki/wiki/Wine
[wine-bug]: https://bugs.winehq.org/show_bug.cgi?id=46972
[git]: https://git-scm.com
[github]: https://github.com
[git-lfs]: https://git-lfs.github.com
[github-lfs-limits]: https://help.github.com/en/github/managing-large-files/about-storage-and-bandwidth-usage
