#include <iostream>
#include <fstream>

#include <QLabel>
#include <QMessageBox>
#include <QGroupBox>
#include <QSettings>
#include <QLayout>
#include <QTemporaryFile>

#include "zlib.h"

#include "main.h"
#include "photoArchiveQt.h"
#include "photoArchive.h"

using std::vector;
using std::map;
using std::endl;


// * start PhotoArchiveWnd

PhotoArchiveWnd::PhotoArchiveWnd( QWidget *, char *)
{
    setMinimumWidth(750);
    setMinimumHeight(550);
    setWindowTitle("photoArchiveQt");

    // * main buttons
    QPushButton *closeButton = new QPushButton(tr("Close"));
    connect(closeButton, SIGNAL(clicked()), this, SLOT(close()));

    QHBoxLayout *buttonsLayout = new QHBoxLayout;
    buttonsLayout->addStretch(1);
    buttonsLayout->addWidget(closeButton);

    // * search fields
    QHBoxLayout *pHLayout1 = new QHBoxLayout;
    QLabel *pSubstringLabel = new QLabel(tr("Substring: "));
    mpSubstringEdit = new QLineEdit;
    mpSearchButton = new QPushButton(tr("Search"));
    connect(mpSearchButton, SIGNAL(clicked()), this, SLOT(searchArchByName()));
    pHLayout1->addWidget(pSubstringLabel);
    pHLayout1->addWidget(mpSubstringEdit);
    pHLayout1->addWidget(mpSearchButton);

    // * list
    mpList=new QListWidget;
    QSizePolicy policyMax;
    policyMax.setHorizontalPolicy(QSizePolicy::Expanding);
    policyMax.setVerticalPolicy(QSizePolicy::Expanding);
    mpList->setSizePolicy(policyMax);
    mpList->setIconSize(QSize(DEFAULT_PREVIEW_SIZE, DEFAULT_PREVIEW_SIZE));

    connect(mpList, SIGNAL(itemClicked(QListWidgetItem*)),
            this, SLOT(itemClicked(QListWidgetItem*)));

    // * status bar
    mpStatusBar=new QLabel;
    mpStatusBar->setTextInteractionFlags(Qt::TextSelectableByMouse);
    
    // * main layout
    QVBoxLayout *mainLayout = new QVBoxLayout;

    mainLayout->addLayout(pHLayout1);
    mainLayout->addWidget(mpList);
    mainLayout->addWidget(mpStatusBar);
    //mainLayout->addStretch(1);
    mainLayout->addSpacing(8);
    mainLayout->addLayout(buttonsLayout);
    setLayout(mainLayout);
}


void
PhotoArchiveWnd::searchArchByName()
{
    bool preview=true;
    log(2, "searching by name");
    QString str=mpSubstringEdit->text();
    if (!str.isEmpty()) {
        disableUserInteraction();
        updateDisplay();
        mpList->clear();
        vector<QFileInfo> files=gpArchive->getFilesContainingStr(str);
        if (files.size()>20) {
            QMessageBox::warning(NULL, tr("Warning"),
                                 tr("Too many results, disabling preview."));
            preview=false;
        }
        for (unsigned int i=0; i<files.size(); i++) {
            QListWidgetItem *pItem=NULL;
            log(2, "filePath=", files[i].filePath(), ", prefixLen=",
                QString::number(gpArchive->getArchPrefixLen()));
            QString nameToDisplay=files[i].filePath().right(
                files[i].filePath().length()-gpArchive->getArchPrefixLen());

            if (preview) {
                // generate preview
                writeToStatusBar(QString("Generating preview for ")+
                                 files[i].fileName());
                updateDisplay();
                QPixmap smallPix=generatePreview(files[i]);
                pItem=new QListWidgetItem(QIcon(smallPix),
                                          nameToDisplay, mpList);
                writeToStatusBar();
            }
            else {
                pItem=new QListWidgetItem(nameToDisplay, mpList);
            }
            mpList->update();
            updateDisplay();
        }
        if (mpList->count()==0) {
            QMessageBox::warning(NULL, tr("Info"), tr("Nothing found"));
        }
        enableUserInteraction();
        updateDisplay();
    }
    else {
        QMessageBox::critical(NULL, tr("Error"), tr("Search string is empty"));
    }
    mpSubstringEdit->setFocus();
    mpSubstringEdit->selectAll();
}


void
PhotoArchiveWnd::disableUserInteraction()
{
    mpSearchButton->setEnabled(false);
    mpSubstringEdit->setEnabled(false);
}


void
PhotoArchiveWnd::enableUserInteraction()
{
    mpSearchButton->setEnabled(true);
    mpSubstringEdit->setEnabled(true);
}


QPixmap
PhotoArchiveWnd::generatePreview(const QFileInfo &rFile)
{
    QPixmap *pBigPix=NULL;
    QPixmap smallPix;
    if (rFile.suffix()=="xcfgz") {
        QString tmpFile;
        log(1, "generating preview for compressed file ", rFile.filePath());
        if (uncompressToTmp(rFile.filePath(), tmpFile)) {
            pBigPix=new QPixmap(tmpFile);
            // remove temp. file
            if (!tmpFile.isEmpty()) {
                unlink(tmpFile.toAscii().constData());
            }
        }
        else {
            QMessageBox::warning(NULL, tr("Warning"),
                                 tr("Error generating preview for a compressed xcf file"));
        }
    }
    else {
        pBigPix=new QPixmap(rFile.filePath());
    }
    if (pBigPix) {
        smallPix=pBigPix->scaled(
            DEFAULT_PREVIEW_SIZE, DEFAULT_PREVIEW_SIZE,
            Qt::KeepAspectRatio, Qt::FastTransformation);
        delete pBigPix;
    }
    return smallPix;
}


/**
 * @param[in] filePath : the xcf.gz file to be uncompressed.
 * @param[out] tmpFilename: returns the filename of the temporary file created
 * (empty on error).
 *
 * @return true = OK, false = error.
 **/
bool
PhotoArchiveWnd::uncompressToTmp(const QString &filePath, QString &tmpFileName)
{
    QTemporaryFile tmpFile;
    tmpFile.setAutoRemove(false);
    
    gzFile fin;
    unsigned bufSize=128*1024;
    char *buf=new char[bufSize];
    unsigned dataRead;
    bool rc=true;
    
    if ((fin=gzopen(filePath.toAscii().constData(), "rb"))==NULL) {
        log(0, "Error opening compressed file ", filePath);
        rc=false;
    }
    if (rc) {
        if (!tmpFile.open()) {
            log(0, "Error opening tmp output file!");
            rc=false;
        }
        else {
            tmpFileName=tmpFile.fileName();
            log(2, "New tmp file created: ", tmpFileName);
        }
    }

    while(rc) {
        dataRead=gzread(fin, buf, bufSize);
        if (0==dataRead)
            break; // EOF
        if (dataRead>0) {
            if (tmpFile.write(buf, dataRead)<=0) {
                log(0, "Error writing to temporary file ", tmpFileName);
                rc=false;
            }
        }
        else {
            log(0, "Error reading from compressed file ", filePath);
            rc=false;
        }
    };
    
    // clean up and return
    delete[] buf;
    gzclose(fin);
    tmpFile.close();
    // on error clear temporary name and delete tmp. file.
    if (!rc) {
        unlink(tmpFileName.toAscii().constData());
        tmpFileName=""; 
    }
    log(2, "Done uncompressing, result is ", rc?"true":"false");
    return rc;
}


void
PhotoArchiveWnd::writeToStatusBar(const QString text)
{
    QString status="<span style=\"text-color: blue; font-weight: bold;\">";
    status+=text;
    status+="</span>";
    mpStatusBar->setText(status);
    mpStatusBar->repaint();
}

void
PhotoArchiveWnd::updateDisplay()
{
    update();
    gApp->processEvents();
}


void
PhotoArchiveWnd::itemClicked(QListWidgetItem *pCurrent)
{
    QString fileName(pCurrent->text());
    log(1, "current item is now ", fileName);
    QString text=fileName.right(fileName.length()-fileName.lastIndexOf("/")-1);
    text+=" (";
    text+=fileName.left(fileName.lastIndexOf("/"));
    text+=")";
    writeToStatusBar(text);
    updateDisplay();
}

