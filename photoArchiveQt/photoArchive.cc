#include <QMessageBox>
#include <QObject>
#include <QDir>
#include <QProcess>

#include "main.h"
#include "photoArchive.h"

using std::vector;

PhotoArchive::PhotoArchive(QString archDir)
{
    vector<QString> excludeList;
    log(2, "rec. scanning dir ", archDir);
    if (!findImagesRec(archDir, excludeList, mFileList)) {
        QMessageBox::critical(NULL, QObject::tr("Error"),
                              QObject::tr("Cannot scan photo archive"));
        mScanOk=false;
    }
    else {
        mScanOk=true;
        mArchivePath=archDir;
        log(2, "archive path=", mArchivePath, ", prefix len=",
            QString::number(getArchPrefixLen()));
    }
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


bool
PhotoArchive::isImageFilename(const QFileInfo &rFile)
{
    QString ext=rFile.suffix().toLower();
    return  ext=="jpg" || ext=="jpeg" || ext=="tif" || ext=="tiff" || ext=="png" || ext=="gif" || ext=="xcf" || ext=="xcfgz" || ext=="nef";
}


bool
PhotoArchive::isMetadataFilename(const QFileInfo &rFile)
{
    QString ext=rFile.suffix().toLower();
    return  ext=="xmp";
}


bool
PhotoArchive::findImagesRec(QString dirName,
                            const vector<QString> &rExcludeList,
                            vector<QFileInfo> &rResult)
{
    QDir dir( dirName );
    log(3, "scanning dir ", dirName);
    dir.setSorting(QDir::DirsFirst | QDir::IgnoreCase);
    if( !dir.exists() )
    {
        return false;
    }
    QString file;
    QString result;
    const QFileInfoList list = dir.entryInfoList();
    if ( list.size()>0 ) {
        const QFileInfo *fi=NULL;
        bool rec=true;
        for (int fIndex=0; fIndex<list.size(); fIndex++) {
            fi=&list[fIndex];
            log(4, "checking file ", fi->fileName());
            if ( !fi->isDir() ) {
                if (isImageFilename(*fi)) {
                    log(3, "adding image file ", fi->filePath());
                    rResult.push_back(*fi);
                }
            }
            if (fi->isDir()) {
                rec=true;
                if ((fi->fileName()==".") || (fi->fileName()=="..") ) {
                    rec=false;
                }
                for (unsigned int i=0; i<rExcludeList.size(); i++) {
                    if ( fi->fileName()==rExcludeList[i]) {
                        rec=false;
                        break;
                    }
                }
            }
            else {
                rec=false;
            }
            if (rec) {
                findImagesRec( fi->filePath(), rExcludeList, rResult);
            }
        }
    }
    return true;
}


std::vector<QFileInfo>
PhotoArchive::getImagesList() const
{
    vector<QFileInfo> result;
    for (unsigned int i=0; i<mFileList.size(); i++) {
        if (isImageFilename(mFileList[i])) {
            result.push_back(mFileList[i]);
        }
    }
    return result;
}


bool
PhotoArchive::compareFiles(const QString file1, const QString file2)
{
    QString prg="cmp";
    QStringList args;
    args<<file1<<file2;
    QProcess *pProc=new QProcess;
    // :fixme: I don't know how to discard output
    //pProc->closeReadChannel(QProcess::StandardOutput);
    //pProc->closeReadChannel(QProcess::StandardError);
    //pProc->setStandardOutputFile("/dev/null");
    //pProc->setStandardErrorFile("/dev/null");
    int rc=pProc->execute(prg, args);
    if (pProc->exitStatus() == QProcess::NormalExit) {
        delete pProc;
        if (0==rc) {
            return true;
        }
        else {
            return false;
        }
    }
    else {
        delete pProc;
        throw QString("Exception in ")+__FUNCTION__;
    }
}

