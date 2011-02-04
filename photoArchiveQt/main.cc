#include <iostream>

#include <QApplication>

#include "main.h"
#include "photoArchiveQt.h"
#include "photoArchive.h"

unsigned int gLogLevel=2;
PhotoArchive *gpArchive=NULL;


void log(unsigned level, QString s1, QString s2, QString s3)
{
    if (gLogLevel>=level) {
        std::cout<<":debug: "<<s1.toStdString()<<s2.toStdString();
        std::cout<<s3.toStdString()<<std::endl;
    }
}


int main( int argc, char **argv )
{
    QApplication a( argc, argv );
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

    // load archive
    gpArchive=new PhotoArchive(DEFAULT_ARCHIVE_PATH);
    // :fixme: check errors
    
    PhotoArchiveWnd *w = new PhotoArchiveWnd();
    w->exec();
}
