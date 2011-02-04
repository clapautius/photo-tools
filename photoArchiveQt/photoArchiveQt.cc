#include <iostream>
#include <fstream>

#include <QLabel>
#include <QMessageBox>
#include <QGroupBox>
#include <QSettings>
#include <QPushButton>
#include <QLayout>

#include "main.h"
#include "photoArchiveQt.h"
#include "photoArchive.h"

using std::vector;
using std::map;
using std::endl;

QSettings gSettings("clapautius", "photoArchiveQt");


static const char*
qstr2cchar(const QString &str)
{
    return str.toUtf8().constData();
}


// * start PhotoArchiveWnd

PhotoArchiveWnd::PhotoArchiveWnd( QWidget *, char *)
{
    //setMinimumWidth(750);
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
    QPushButton *searchButton = new QPushButton(tr("Search"));
    connect(searchButton, SIGNAL(clicked()), this, SLOT(searchArchByName()));
    pHLayout1->addWidget(pSubstringLabel);
    pHLayout1->addWidget(mpSubstringEdit);
    pHLayout1->addWidget(searchButton);

    // * list
    mpList=new QListWidget;
    
    // * main layout
    QVBoxLayout *mainLayout = new QVBoxLayout;

    mainLayout->addLayout(pHLayout1);
    mainLayout->addWidget(mpList);
    mainLayout->addStretch(1);
    mainLayout->addSpacing(8);
    mainLayout->addLayout(buttonsLayout);
    setLayout(mainLayout);
}


void
PhotoArchiveWnd::searchArchByName()
{
    log(2, "searching by name");
    QString str=mpSubstringEdit->text();
    if (!str.isEmpty()) {
        mpList->clear();
        vector<QFileInfo> files=gpArchive->getFilesContainingStr(str);
        for (unsigned int i=0; i<files.size(); i++) {
            mpList->addItem(QString(files[i].filePath()));
        }
    }
    else {
        QMessageBox::critical(NULL, tr("Error"), tr("Search string is empty"));
    }
}


