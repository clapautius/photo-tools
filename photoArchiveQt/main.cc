#include <iostream>
#include <vector>

#include <QSettings>

#include "main.h"
#include "photoArchiveQt.h"
#include "photoArchive.h"

using std::vector;

unsigned int gLogLevel=2;
PhotoArchive *gpArchive=NULL;
QApplication *gApp=NULL;

QSettings gSettings("clapautius", "photoArchiveQt");

void log(unsigned level, QString s1, QString s2, QString s3, QString s4)
{
    if (gLogLevel>=level) {
        std::cout<<":debug: "<<s1.toStdString()<<s2.toStdString();
        std::cout<<s3.toStdString()<<s4.toStdString()<<std::endl;
    }
}


const char*
qstr2cchar(const QString &str)
{
    return str.toUtf8().constData();
}



/**
 * Read config settings for collection directory and collection exclude list.
 * If the settings do not exist, they are saved (using some default values).
 **/
static void
getConfigPaths(QString &rArchiveDir, vector<QString> &rArchiveExcludeList)
{
    QString dir;
    QString str;
    vector<QString> excludeList;
    QStringList xListTmp;
    dir=gSettings.value("photoArchiveDir").toString();
    if (dir.isEmpty()) {
        gSettings.setValue("photoArchiveDir", QVariant(QString("/home/me/Pictures")));
    }
    dir=gSettings.value("photoArchiveDir").toString();
    rArchiveDir=dir;

    xListTmp=gSettings.value("photoArchiveExcludeList").toString().split(",", QString::SkipEmptyParts);
    for (int i=0; i<xListTmp.size(); i++) {
        rArchiveExcludeList.push_back(xListTmp[i]);
    }
    log(1, "archive dir is ", rArchiveDir);
}


int main( int argc, char **argv )
{
    gApp=new QApplication( argc, argv );
    /*
    if (argc<=1) {
        QMessageBox::critical(NULL, "Error", "No image file specified.");
        return 1;
    }
    else
    */
    if (argc>1) {
        if (strcmp(argv[1], "-h")==0 || strcmp(argv[1], "--help")==0) {
            std::cout<<"photoArchiveQt ver. "<<PHOTO_ARCHIVE_QT_VER<<std::endl;
            std::cout<<"Usage: photoArchiveQt"<<std::endl;
            return 0;
        }
    }

    QString dir;
    vector<QString> excludeList;
    getConfigPaths(dir, excludeList);

    // load archive
    gpArchive=new PhotoArchive(dir);
    if (gpArchive->isOk()) {
        PhotoArchiveWnd *w = new PhotoArchiveWnd();
        w->exec();
        return 0;
    }
    else {
        return 1;
    }
}
