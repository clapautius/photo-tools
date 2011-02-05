#ifndef PHOTO_ARCHIVE_H
#define PHOTO_ARCHIVE_H

#include <vector>

#include <QString>
#include <QFileInfo>

class PhotoArchive
{
public:
    PhotoArchive(QString aArchDir);

    bool isOk() const
    {
        return mScanOk;
    }

    unsigned short getArchPrefixLen() const
    {
        return mArchivePath.length();
    }
    
    std::vector<QFileInfo> getFilesContainingStr(const QString &rSubStr) const;

    std::vector<QFileInfo> getImagesList() const;
    
    static bool isImageFilename(const QFileInfo& file);

    static bool isMetadataFilename(const QFileInfo& file);

    static bool compareFiles(const QString file1, const QString file2);

    bool findImagesRec(QString dirName, const std::vector<QString> &excludeList,
                       std::vector<QFileInfo> &result);
    
private:

    bool mScanOk;

    QString mArchivePath;
    
    std::vector<QFileInfo> mFileList;
};

#endif
