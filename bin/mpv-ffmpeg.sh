#!/bin/sh
#
# See: https://raw.githubusercontent.com/NapoleonWils0n/nixos-bin/master/dash-ffmpeg
# UPDATE: cumbersome; instead use this: "yt-dlp \"%s\" -o - | mpv -
#

#===============================================================================
# mpv-ffmpeg
# combine audio and video streams witth ffmpeg and pipe into mpv
#===============================================================================

# dependencies:
# ffmpeg mpv yt-dlp awk

#===============================================================================
# script usage
#===============================================================================

usage ()
{
# if argument passed to function echo it
[ -z "${1}" ] || echo "! ${1}"
# display help
echo "\
# combine audio and video streams witth ffmpeg and pipe into mpv

$(basename "$0") -i input
-i input"
exit 2
}


#===============================================================================
# error messages
#===============================================================================

INVALID_OPT_ERR='Invalid option:'
REQ_ARG_ERR='requires an argument'
WRONG_ARGS_ERR='wrong number of arguments passed to script'


#===============================================================================
# check the number of arguments passed to the script
#===============================================================================

[ $# -gt 0 ] || usage "${WRONG_ARGS_ERR}"


#===============================================================================
# getopts check the options passed to the script
#===============================================================================

while getopts ':i:h' opt
do
  case ${opt} in
     i) input="${OPTARG}";;
     h) usage;;
     \?) usage "${INVALID_OPT_ERR} ${OPTARG}" 1>&2;;
     :) usage "${INVALID_OPT_ERR} ${OPTARG} ${REQ_ARG_ERR}" 1>&2;;
  esac
done
shift $((OPTIND-1))


#===============================================================================
# function
#===============================================================================

combine () {
url=$(yt-dlp -g --no-playlist "${input}")
video_url=$(echo "${url}" | awk 'BEGIN{ RS ="" ; FS ="\n" }{print $1}')
audio_url=$(echo "${url}" | awk 'BEGIN{ RS ="" ; FS ="\n" }{print $2}')

# ffmpeg join audio and video and stream
# -vf scale=1920:-1 \ 
ffmpeg \
-hide_banner \
-stats -v panic \
-re \
-i "${video_url}" \
-i "${audio_url}" \
-c:a copy -c:v copy \
-tune zerolatency \
-map 0:0 -map 1:0 \
-f matroska pipe: | mpv - --cache 2048 --no-terminal --fs --fs-screen=1
}


#===============================================================================
# run function
#===============================================================================

combine
