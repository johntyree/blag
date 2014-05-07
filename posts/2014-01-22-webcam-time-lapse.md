-----------------------------------------
title: Time-Lapse Photography with FFMPEG
tags: Hack ffmpeg piet
-----------------------------------------

We got a dog recently and as you can see, he's adorable,

<img src=/../images/piet01.jpg />

Unfortunately, he has a bit of separation anxiety when we go out. It's not
uncommon, but can lead to some pretty destructive behavior, destroying
furniture, urinating everywhere, things no one wants their dog to do.

To get a better feel for how he behaves when no one is around to see, I had the
idea to set up some cameras around the house. Everything was great until we
realized that we don't *have* cameras. We have laptops with webcams. On top of
that, we have no internet in our apartment and nothing installed for taking
photos with webcams.

There are a few options available for this, including some very nice, polished
applications like [VLC](http://videolan.net), but that's no fun. To be fair,
VLC doesn't do *quite* what I (thought) I wanted anyway. I settled on
[`ffmpeg`](http://ffmpeg.org) as a good, lightweight solution for capturing
frames from a generic video source.

If you've ever tried to do anything with `ffmpeg`, then you already know the
kind of wizardry it takes to do even the most trivial tasks. Seriously deep
magics. It's not that it's poorly designed (well maybe...), it's just that
there's *so damn much it can do*. Not wanting to give up, I dug through the
docs and came up with the following incantation:

```bash
ffmpeg -f video4linux2 -i /dev/video0 -vframes 1 "output.jpg"
```

Here's how it works:
 * access the webcam (`/dev/video0`)
 * using the `v4l2` subsystem (`-f video4linux2`)
 * captures a single frame (`-vframes 1`)
 * and saves it as a `jpg` file.

From here it was smooth sailing. All I had left to do was write a loop to take
snapshots and ensure that each shot got a unique name so they wouldn't
overwrite each other.

I eventually settled on a little bash script that handles things perfectly. It
allows you to select between multiple cams and names the file according to
the current date and time.

```bash
#!/bin/bash

# Choose a camera
NUM=${1:-"0"}

# Create the filename
DATETIME=$(date +%F_%T.%N | rev | cut -c 8- | rev)
OUTPUTDIR="/home/john/Pictures/Webcam-Snapshots"
OUTFILE="$OUTPUTDIR/CAM${NUM}-${DATETIME}.jpg"

# Take the photo
ffmpeg -f video4linux2 -i /dev/video${NUM} -vframes 1 "$OUTFILE"
```

Now just put a call to this in a loop like so:

`while true; do sleep 0.2; webcam-snapshot; done` and that's it! It will
happily chug along, taking photos at set intervals and saving them in order to
disk. Create an animated `gif` of your new photos with

```bash
ffmpeg TODO
```

Here's an example of our dog maxin' and relaxin'.

<img src=../images/piet-in-bed.gif />
