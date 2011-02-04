#include "photoArchive.h"

using std::vector;

PhotoArchive::PhotoArchive(const char *pArchDir)
{
    mFileList.push_back(QFileInfo("/etc/passwd"));
}


vector<QFileInfo>
PhotoArchive::getFilesContainingStr(const QString &rSubStr) const
{
    vector<QFileInfo> result;
    for (unsigned int i=0; i<mFileList.size(); i++) {
        if (mFileList[i].fileName().contains(rSubStr)) {
            result.push_back(mFileList[i]);
        }
    }
    return result;
}
