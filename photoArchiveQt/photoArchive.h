#ifndef PHOTO_ARCHIVE_H
#define PHOTO_ARCHIVE_H

#include <vector>

#include <QString>
#include <QFileInfo>

class PhotoArchive
{
public:
    PhotoArchive(const char *pArchDir);

    std::vector<QFileInfo> getFilesContainingStr(const QString &rSubStr) const;

private:

    std::vector<QFileInfo> mFileList;
};

#endif
