#ifndef PHOTO_ARCHIVE_MAIN_H
#define PHOTO_ARCHIVE_MAIN_H

#include <QApplication>
#include <QString>

#define PHOTO_ARCHIVE_QT_VER "1.1" // :release:

#define DEFAULT_ARCHIVE_PATH "/home/me/Pictures"

#define DEFAULT_PREVIEW_SIZE 100

void log(unsigned level, QString s1, QString s2="", QString s3="",
         QString s4="");

const char* qstr2cchar(const QString &str);

class PhotoArchive;
extern PhotoArchive *gpArchive;
extern QApplication *gApp;

#endif
