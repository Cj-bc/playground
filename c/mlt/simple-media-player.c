/*
  simple-media-player -- my practice program to use MLT library

  Copyright 2023 Cj-bc a.k.a Cj.bc_sd

  Also note that I borrowed some of parts from
  - https://github.com/mltframework/mlt/blob/769cf9dd0aec54221f8fb8cbc4b4f50630136578/src/melt/melt.c#L983-L988
  - https://www.mltframework.org/docs/framework/
  
  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version. 

  This program is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
  General Public License for more details. 

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <https://www.gnu.org/licenses/>. 


  This program is based on
  https://www.mltframework.org/docs/framework/,
  with modification to make it work with current mlt library and
  graceful exit from melt code above.
*/
#include <stdio.h>
#include <unistd.h>
#include <framework/mlt.h>
#include <signal.h>

// In order to gracefully close consumer on signal, I have to make it static
static mlt_consumer consumer;

void signal_handler(int signum) {
  if (consumer) {
    mlt_properties prop = MLT_CONSUMER_PROPERTIES (consumer);
    mlt_properties_set_int(prop, "done", 1);
  }
}


mlt_playlist make_playlist(mlt_profile profile, int pathsLen, char* paths[]) {
  mlt_playlist playlist = mlt_playlist_init();
  for (int i=1; i < pathsLen; i++) {
    mlt_producer prod = mlt_factory_producer(profile, NULL, paths[i]);
    mlt_playlist_append(playlist, prod);
    mlt_producer_close(prod);
  }

  return playlist;
}

int main (int argc, char *argv[]) {
  if (argc <= 1) {
    fprintf(stdout, "simple-media-player: plays movies in serial form\nsimple-media-player [MOVIE_PATH ...]\n");
    return 0;
  }
  mlt_repository repo = mlt_factory_init(NULL);

  if (!repo) {
    fprintf(stderr, "Failed to initialize factory");
    return -1;
  }

  mlt_profile profile = mlt_profile_init(NULL);

  // Creating playlist
  mlt_playlist playlist = make_playlist(profile, argc, argv);

  // Re create playlist with proper profile
  mlt_profile_from_producer(profile, MLT_PLAYLIST_PRODUCER(playlist));
  mlt_playlist_close(playlist);
  playlist = make_playlist(profile, argc, argv);
  }

  for (int i = 0; i < mlt_playlist_count(playlist); i++) {
    mlt_playlist_clip_info info;
    mlt_playlist_get_clip_info(playlist, &info, i);
    fprintf(stdout, "playlist[%i]: %s\n", i, info.resource);
  }
  mlt_playlist_close(playlist);
  mlt_factory_close();
}
