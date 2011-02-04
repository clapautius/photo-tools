#ifndef PHOTO_ARCHIVE_MAIN_H
#define PHOTO_ARCHIVE_MAIN_H

#include <QString>

#define DEFAULT_ARCHIVE_PATH "/home/me/Pictures"

void log(unsigned level, QString s1, QString s2="", QString s3="");

class PhotoArchive;
extern PhotoArchive *gpArchive;

#endif
