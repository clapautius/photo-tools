#include <iostream>
#include <fstream>

#include <QApplication>
#include <QMessageBox>
#include <QGroupBox>
#include <QSettings>
#include <QDir>

#include "photo-collection-qt.h"

using std::vector;
using std::map;
using std::endl;

//#define FNAME_STYLE_BEGIN "<span style=\"font-weight: bold; text-decoration: underline;\">"
#define FNAME_STYLE_BEGIN "<span style=\"text-decoration: underline;\">"
#define FNAME_STYLE_END "</span>"

unsigned int gLogLevel=2;

QSettings gSettings("clapautius", "photoCollectionQt");

void log(unsigned level, QString s1, QString s2="", QString s3="");


/// escapes special XML chars and returns a QString
static QString
toXmlString(const QString &qstr)
{
    QString localStr=qstr;
    localStr.replace("&", "&amp;");
    localStr.replace("<", "&lt;");
    localStr.replace(">", "&gt;");
    localStr.replace("\"", "&quot;");
    localStr.replace("'", "&apos;");
    return localStr;
}


/**
 * Replace the filename extension with '.xml' extension.
 **/
static QString
getXmlFilename(const QFileInfo file)
{
    return file.path()+"/"+file.completeBaseName()+".xml";
}


/**
 * Read config settings for collection directory and collection exclude list.
 * If the settings do not exist, they are saved (using some default values).
 **/
static void
getConfigPaths(QString &rCollectionDir, vector<QString> &rCollectionExcludeList)
{
    QString dir;
    QString str;
    vector<QString> excludeList;
    QStringList xListTmp;
    dir=gSettings.value("collectionDir").toString();
    if (dir.isEmpty()) {
        gSettings.setValue("collectionDir", QVariant(QString("/home/me/var/alternatives/colectieFoto")));
    }
    dir=gSettings.value("collectionDir").toString();
    rCollectionDir=dir;

    str=gSettings.value("collectionExcludeList").toString();
    if (str.isEmpty()) {
        gSettings.setValue("collectionExcludeList", QVariant(QString("00-lada")));
    }
    xListTmp=gSettings.value("collectionExcludeList").toString().split(",", QString::SkipEmptyParts);
    for (int i=0; i<xListTmp.size(); i++) {
        rCollectionExcludeList.push_back(xListTmp[i]);
    }
    log(1, "collection dir is ", rCollectionDir);
    log(1, "collection exclude dir is ", rCollectionExcludeList[0]);
}


static bool
isImageFilename(QFileInfo file)
{
    QString ext=file.suffix().toLower();
    return  ext=="jpg" || ext=="jpeg" || ext=="tif" || ext=="tiff" || ext=="png" || ext=="gif";
}


static QString
findImageRec(QString dirName, vector<QString> &rExcludeList)
{
    QDir dir( dirName );
    log(3, "rec. scanning dir ", dirName);
    dir.setSorting(QDir::DirsFirst | QDir::IgnoreCase);
    if( !dir.exists() )
    {
        return "";
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
                    log(3, "checking xml file for ", fi->filePath());
                    QFileInfo xmlFile;
                    xmlFile.setFile(getXmlFilename(*fi));
                    log(3, "xml filename is ", xmlFile.filePath());
                    if (!xmlFile.isFile()) {
                        log(1, "found a file without xml: ", fi->filePath());
                        return fi->filePath();
                    }
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
                result=findImageRec( fi->filePath(), rExcludeList );
                if (!result.isEmpty())
                    return result;
            }
        }
    }
    return "";
}


static QString
getFirstUntaggedImage(QString dir, vector<QString> excludeList)
{
    log(1, "looking for an untagged image file in dir: ", dir);
    log(2, "exclude list:");
    for (unsigned int i=0; i<excludeList.size(); i++) {
        log(2, excludeList[i]);
    }
    log(2, "end exclude list:");
    QString result=findImageRec(dir, excludeList);
    if (result.isEmpty()) {
        log(1, "no untagged image file found");
    }
    else {
        log(1, "found untagged image file: ", result);
    }
    return result;
}


// * start PhotoCollectionWnd

PhotoCollectionWnd::PhotoCollectionWnd( QWidget *, char *)
{
    mTagLayoutPos=0;
    mSaved=false;

    setMinimumWidth(750);
    setWindowTitle(" (photoAIC) ");
    
    // * buttons
    QPushButton *closeButton = new QPushButton(tr("Close"));
    connect(closeButton, SIGNAL(clicked()), this, SLOT(close()));

    QPushButton *applyButton = new QPushButton(tr("Save XML"));
    connect(applyButton, SIGNAL(clicked()), this, SLOT(updatePhotoXml()));

    QPushButton *previewButton = new QPushButton(tr("Preview XML"));
    connect(previewButton, SIGNAL(clicked()), this, SLOT(previewPhotoXml()));

    QPushButton *lookForImageButton = new QPushButton(tr("Look for untagged image"));
    connect(lookForImageButton, SIGNAL(clicked()), this, SLOT(lookForImage()));

    QHBoxLayout *buttonsLayout = new QHBoxLayout;
    buttonsLayout->addStretch(1);
    buttonsLayout->addWidget(lookForImageButton);
    buttonsLayout->addStretch(1);
    buttonsLayout->addWidget(previewButton);
    buttonsLayout->addStretch(1);
    buttonsLayout->addWidget(applyButton);
    buttonsLayout->addStretch(1);
    buttonsLayout->addWidget(closeButton);

    // * edit fields

    QHBoxLayout *pHLayout1 = new QHBoxLayout;
    QVBoxLayout *pMainFieldsLabelsLayout = new QVBoxLayout;
    QVBoxLayout *pMainFieldsEditLayout = new QVBoxLayout;
    pHLayout1->addLayout(pMainFieldsLabelsLayout);
    pHLayout1->addLayout(pMainFieldsEditLayout);

    // * name (read-only)
    QLabel *pNameLabel = new QLabel(tr("Filename"));
    mpNameField = new QLabel;
    mpNameField->setAlignment(Qt::AlignHCenter);
    //mpNameField->setReadOnly(true);
    pMainFieldsLabelsLayout->addWidget(pNameLabel);
    pMainFieldsEditLayout->addWidget(mpNameField);
    
    // * URL
    QLabel *pUrlLabel = new QLabel(tr("URL"));
    mpUrlEdit = new QLineEdit;
    pMainFieldsLabelsLayout->addWidget(pUrlLabel);
    pMainFieldsEditLayout->addWidget(mpUrlEdit);

    QHBoxLayout *pHLayout2 = new QHBoxLayout;
    QVBoxLayout *pVLayout21 = new QVBoxLayout;
    QVBoxLayout *pVLayout22 = new QVBoxLayout;
    QVBoxLayout *pVLayout23 = new QVBoxLayout;
    QVBoxLayout *pVLayout24 = new QVBoxLayout;
    pHLayout2->addLayout(pVLayout21);
    pHLayout2->addLayout(pVLayout22);
    pHLayout2->addLayout(pVLayout23);
    pHLayout2->addLayout(pVLayout24);

    // pVLayout2x are not strictly necesarry, they might be useful in the
    // future, if other fields will be added, to align them vertically.
    
    // * author
    QLabel *pAuthorLabel = new QLabel(tr("Author"));
    mpAuthorEdit = new QLineEdit;
    pVLayout21->addWidget(pAuthorLabel);
    pVLayout22->addWidget(mpAuthorEdit);
    
    // * title
    QLabel *pTitleLabel = new QLabel(tr("Title"));
    mpTitleEdit = new QLineEdit;
    pVLayout23->addWidget(pTitleLabel);
    pVLayout24->addWidget(mpTitleEdit);

    // * type
    QGroupBox *pTypeBox = new QGroupBox;
    QHBoxLayout *pTypeLayout = new QHBoxLayout;
    mpAmateurBtn= new QRadioButton(tr("amateur"), pTypeBox);
    mpAmateurBtn->setChecked(true);
    mpEstablishedBtn= new QRadioButton(tr("established"), pTypeBox);
    mpPaintingBtn= new QRadioButton(tr("painting"), pTypeBox);
    pTypeLayout->addWidget(mpAmateurBtn);
    pTypeLayout->addWidget(mpEstablishedBtn);
    pTypeLayout->addWidget(mpPaintingBtn);
    pTypeBox->setLayout(pTypeLayout);
    
    // * details & exif
    QHBoxLayout *pHLayout3 = new QHBoxLayout;
    QGroupBox *pDetailsBox = new QGroupBox(tr("Details"));
    mpDetailsEdit = new QTextEdit;
    mpDetailsEdit->setAcceptRichText(false);
    mpDetailsEdit->setLineWrapMode(QTextEdit::NoWrap);
    QHBoxLayout *pDetailsLayout = new QHBoxLayout;
    pDetailsLayout->addWidget(mpDetailsEdit);
    pDetailsBox->setLayout(pDetailsLayout);

    QGroupBox *pExifBox = new QGroupBox(tr("EXIF"));
    mpExifEdit = new QTextEdit;
    mpExifEdit->setAcceptRichText(false);
    mpExifEdit->setLineWrapMode(QTextEdit::NoWrap);
    QHBoxLayout *pExifLayout = new QHBoxLayout;
    pExifLayout->addWidget(mpExifEdit);
    pExifBox->setLayout(pExifLayout);

    pHLayout3->addWidget(pDetailsBox);
    pHLayout3->addWidget(pExifBox);

    // * tags

    // * tags checkboxes
    QHBoxLayout *pHLayout4 = new QHBoxLayout;
    for (unsigned int i=0; i<TAGS_NO_COLUMNS; i++) {
        mTagsLayouts.push_back(new QVBoxLayout);
        pHLayout4->addLayout(mTagsLayouts[i]);
    }

    QGroupBox *pTagsGroup = new QGroupBox;
    pTagsGroup->setLayout(pHLayout4);

    addTagCheckbox("black-white");
    addTagCheckbox("border");
    addTagCheckbox("didactic");
    addTagCheckbox("didactic-pp");
    addTagCheckbox("duo-tone"); // 5
    addTagCheckbox("fog-rain");
    addTagCheckbox("fragments");
    addTagCheckbox("glamour");
    addTagCheckbox("horses");
    addTagCheckbox("insolit"); // 10
    addTagCheckbox("landscape");
    addTagCheckbox("macro-closeup");
    addTagCheckbox("minimalist");
    addTagCheckbox("nature");
    addTagCheckbox("night"); // 15
    addTagCheckbox("people");
    addTagCheckbox("photojournalism");
    addTagCheckbox("places");
    addTagCheckbox("portrait");
    addTagCheckbox("product"); // 20
    addTagCheckbox("rural");
    addTagCheckbox("safety-moderate");
    addTagCheckbox("sunset-sunrise");
    addTagCheckbox("studio");
    addTagCheckbox("tree");
    addTagCheckbox("urban");
    addTagCheckbox("water-river-sea");
    addTagCheckbox("wildlife");
    addTagCheckbox("winter-snow");
    addTagCheckbox(""); // placeholder // 30

    // * other tags
    QHBoxLayout *pHLayout5 = new QHBoxLayout;
    QLabel *pOtherTagsLabel = new QLabel(tr("Other tags"));
    mpOtherTagsEdit = new QLineEdit;
    pHLayout5->addWidget(pOtherTagsLabel);
    pHLayout5->addWidget(mpOtherTagsEdit);
    
    // * main layout
    QVBoxLayout *mainLayout = new QVBoxLayout;

    mainLayout->addLayout(pHLayout1);
    mainLayout->addLayout(pHLayout2);
    mainLayout->addWidget(pTypeBox);
    mainLayout->addLayout(pHLayout3);
    mainLayout->addWidget(pTagsGroup);
    mainLayout->addLayout(pHLayout5);
    mainLayout->addStretch(1);
    mainLayout->addSpacing(8);
    mainLayout->addLayout(buttonsLayout);
    setLayout(mainLayout);
}


void
PhotoCollectionWnd::setFileNameField(const QFileInfo &rFile)
{
    QString labelText=FNAME_STYLE_BEGIN;
    if (rFile.filePath().isEmpty()) {
        labelText+="-";
    }
    else {
        labelText+=QString("...")+rFile.path().right(5)+QString("/");
        labelText+=rFile.fileName();
    }
    labelText+=FNAME_STYLE_END;
    mpNameField->setText(labelText);
}


void
PhotoCollectionWnd::setPhotoFileName(QFileInfo photoFileName)
{
    mPhotoFileName=photoFileName;
    log(2, "current image file path is: ",
        mPhotoFileName.filePath());
    log(2, "current image filename is: ",
        mPhotoFileName.fileName());
    setFileNameField(mPhotoFileName);
}


/**
 * @param[in] checkboxText : text of the checkbox. If empty, a placeholder (a
 * QLabel) will be placed instead.
 **/
void
PhotoCollectionWnd::addTagCheckbox(const char* checkboxText)
{
    QVBoxLayout *pLayoutToUse=NULL;
    pLayoutToUse=mTagsLayouts[mTagLayoutPos];
    if (checkboxText[0]) {
        QCheckBox *pTagCheckbox=new QCheckBox(checkboxText);
        pLayoutToUse->addWidget(pTagCheckbox);
        mStandardTags[checkboxText]=pTagCheckbox;
    }
    else { // empty string = place holder - to keep things aligned
        QLabel *pLabel=new QLabel("");
        pLayoutToUse->addWidget(pLabel);
    }
    mTagLayoutPos++;
    if (mTagLayoutPos>=TAGS_NO_COLUMNS)
        mTagLayoutPos=0;
}


QString
PhotoCollectionWnd::getUrl() const
{
    return mpUrlEdit->text();
}


QString
PhotoCollectionWnd::getAuthor() const
{
    return mpAuthorEdit->text();
}


QString
PhotoCollectionWnd::getTitle() const
{
    return mpTitleEdit->text();
}


QString
PhotoCollectionWnd::getType() const
{
    if (mpAmateurBtn->isChecked()) {
        return "amateur";
    }
    else if (mpEstablishedBtn->isChecked()) {
        return "established";
    }
    else {
        return "classicPainting";
    }
}


QString
PhotoCollectionWnd::getDetails() const
{
    return mpDetailsEdit->toPlainText();
}


QString
PhotoCollectionWnd::getExif() const
{
    return mpExifEdit->toPlainText();
}


vector<QString>
PhotoCollectionWnd::getAllTags() const
{
    vector<QString> result;
    QStringList userTags;

    // collect standard tags (verify checkboxes)
    std::map<QString, QCheckBox*>::const_iterator it;
    for ( it=mStandardTags.begin() ; it != mStandardTags.end(); it++ ) {
        if ((*it).second->isChecked()) {
            result.push_back((*it).first);
        }
    }

    // collect user tags (from edit line)
    userTags=mpOtherTagsEdit->text().split(" ", QString::SkipEmptyParts);
    for (int i=0; i<userTags.size(); i++) {
        result.push_back(toXmlString(userTags[i]));
    }
    
    log(2, "tags:");
    for (unsigned int i=0; i<result.size(); i++) {
        log(2, "  ", result[i]);
    }

    return result;
}


QString
PhotoCollectionWnd::generateXmlText() const
{
    QString xmlOutput;
    QString str;
    QString nl("\n");
    if (mPhotoFileName.filePath().isEmpty()) {
        QMessageBox::critical(NULL, tr("Error"), tr("No image file specified"));
        return "";
    }
    xmlOutput="<?xml version=\"1.0\" encoding=\"utf-8\" standalone='yes'?>";
    xmlOutput+=nl+"<!DOCTYPE photo SYSTEM \"http://sfd.ro/xml/photo.dtd\">"+nl;
    xmlOutput+=nl+QString("<photo type=\"")+getType()+"\">"+nl;
    xmlOutput+=QString("    <filename>")+mPhotoFileName.fileName()+
        "</filename>"+nl;
    str=toXmlString(getAuthor());
    if (str.isEmpty()) {
        QMessageBox::critical(NULL, tr("Error"), tr("Author not specified"));
        return "";
    }
    else {
        xmlOutput+=QString("    <author>")+str+"</author>"+nl;
    }
    str=toXmlString(getUrl());
    if (str.isEmpty()) {
        QMessageBox::critical(NULL, tr("Error"), tr("URL not specified"));
        return "";
    }
    else {
        xmlOutput+=QString("    <source>")+str+"</source>"+nl;
    }
    str=toXmlString(getTitle());
    if (!str.isEmpty()) {
        xmlOutput+=QString("    <title>")+str+"</title>"+nl;
    }
    str=toXmlString(getDetails());
    if (!str.isEmpty()) {
        xmlOutput+=QString("    <details>")+str+nl+"    </details>"+nl;
    }
    str=toXmlString(getExif());
    if (!str.isEmpty()) {
        xmlOutput+=QString("    <exif>")+str+nl+"    </exif>"+nl;
    }
    vector<QString> tags=getAllTags();
    for (unsigned int i=0; i<tags.size(); i++) {
        xmlOutput+=QString("    <tag>")+tags[i]+"</tag>"+nl;
    }

    // date
    char dateBuf[11];
    time_t now=time(NULL);
    strftime(dateBuf, sizeof(dateBuf), "%Y-%m-%d", localtime(&now));
    xmlOutput+=QString("    <date>")+dateBuf+"</date>"+nl;
    
    xmlOutput+=QString("</photo>")+nl;
    log(3, "xml text:");
    log(3, xmlOutput);
    log(3, "end xml text");
    return xmlOutput;
}


void
PhotoCollectionWnd::updatePhotoXml()
{
    log(1, "updating XML for file ", mPhotoFileName.filePath());
    std::ofstream fout;
    QString xmlText=generateXmlText();
    if (xmlText.isEmpty()) {
        return;
    }
    QString xmlPath = getXmlFilename(mPhotoFileName);
    log(1, "creating xml file with name ", xmlPath);
    if (QFile::exists(xmlPath)) {
        if (QMessageBox::Yes != QMessageBox::question(
                NULL, tr("Confirmation"),
                tr("XML file already exists. Overwrite?"),
                QMessageBox::Yes, QMessageBox::No)) {
            return;
        }
        else {
            log(1, "overwriting xml file");
        }
    }
    fout.open(xmlPath.toStdString().c_str(), std::ios::out);
    if (!fout.good()) {
        QMessageBox::critical(NULL, tr("Error"),
                              tr("Cannot open XML file for writing."));
        return;
    }
    // it's easier to write raw data from a std::string
    std::string s = xmlText.toStdString();
    if (s.size() <= 0) {
        QMessageBox::critical(NULL, tr("Error"), tr("Empty XML output"));
        return;
    }
    fout.write(s.c_str(), s.size());
    if (!fout.good()) {
        QMessageBox::critical(NULL, tr("Error"), tr("Error writing XML data."));
        log(0, "error writing XML data");
        fout.close();
        return;
    }
    fout.close();
    log(1, "XML OK");
    mSaved=true;
    QMessageBox::information(NULL, tr(" (photoAIC) - OK"),
                             tr("XML file succesfully saved."));
    return;
}


void
PhotoCollectionWnd::previewPhotoXml()
{
    vector<QString> tags;
    log(2, "previewing XML");
    QString xmlText=generateXmlText();
    if (xmlText.isEmpty()) {
        return;
    }
    PhotoColPreviewWnd *pWnd=new PhotoColPreviewWnd(xmlText);
    pWnd->exec();
}


void
PhotoCollectionWnd::resetFields()
{
    mSaved=false;
    setFileNameField(mPhotoFileName);
    mpUrlEdit->setText("");
    mpAuthorEdit->setText("");
    mpTitleEdit->setText("");
    mpAmateurBtn->setChecked(true);
    mpDetailsEdit->setText("");
    mpExifEdit->setText("");
    mpOtherTagsEdit->setText("");
    std::map<QString, QCheckBox*>::iterator it;
    it=mStandardTags.begin();
    while (it != mStandardTags.end()) {
        (*it).second->setChecked(false);
        it++;
    }
}


void
PhotoCollectionWnd::lookForImage()
{
    if (!mPhotoFileName.filePath().isEmpty() && !mSaved) {
        if (QMessageBox::No ==
            QMessageBox::question(NULL, tr("Warning"),
                                  tr("XML not saved. Do you really want to change image?"),
                                  QMessageBox::Yes, QMessageBox::No))
        {
            return;
        }
    }
    QString dir;
    vector<QString> excludeList;
    getConfigPaths(dir, excludeList);
    QString firstImageFount=getFirstUntaggedImage(dir, excludeList);
    if (!firstImageFount.isEmpty()) {
        mPhotoFileName.setFile(firstImageFount);
        resetFields();
    }
    else {
        QMessageBox::warning(NULL, tr("Warning"),
                             tr("No untagged image found"));
    }
}


void
PhotoCollectionWnd::closeEvent(QCloseEvent *pEvent)
{
    log(1, "Checking if saved");
    if (!mPhotoFileName.filePath().isEmpty() && !mSaved) {
        if (QMessageBox::No ==
            QMessageBox::question(NULL, tr("Warning"),
                                  tr("XML not saved. Do you really want to quit?"),
                                  QMessageBox::Yes, QMessageBox::No))
        {
            pEvent->ignore();
            return;
        }
    }
    pEvent->accept();
}


// end PhotoCollectionWnd class functions


// * PhotoColPreviewWnd class functions

PhotoColPreviewWnd::PhotoColPreviewWnd(QString xmlText)
    : mXmlText(xmlText)
{
    resize(600, 400);
    setWindowTitle(" (photoAIC) - XML preview");
    QPushButton *pCloseButton = new QPushButton(tr("Close"));
    connect(pCloseButton, SIGNAL(clicked()), this, SLOT(close()));

    QVBoxLayout *pMainLayout = new QVBoxLayout;
    
    QHBoxLayout *pButtonsLayout = new QHBoxLayout;
    pButtonsLayout->addStretch(1);
    pButtonsLayout->addWidget(pCloseButton);

    QTextEdit *pXmlEdit=new QTextEdit;
    QSizePolicy policyMax;
    policyMax.setHorizontalPolicy(QSizePolicy::Expanding);
    policyMax.setVerticalPolicy(QSizePolicy::Expanding);
    pXmlEdit->setSizePolicy(policyMax);
    pXmlEdit->setReadOnly(true);
    pXmlEdit->setPlainText(mXmlText);
    
    pMainLayout->addWidget(pXmlEdit);
    pMainLayout->addSpacing(12);
    pMainLayout->addLayout(pButtonsLayout);
    setLayout(pMainLayout);
}



// * end PhotoColPreviewWnd class functions

void log(unsigned level, QString s1, QString s2, QString s3)
{
    if (gLogLevel>=level) {
        std::cout<<":debug: "<<s1.toStdString()<<s2.toStdString();
        std::cout<<s3.toStdString()<<endl;
    }
}



int main( int argc, char **argv )
{
    QFileInfo photoFileName;
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
            std::cout<<"photoCollectionQt ver. "<<PHOTO_COLLECTION_QT_VER<<endl;
            std::cout<<"Usage: photoCollectionQt [imageFile]"<<endl;
            return 0;
        }
        photoFileName.setFile(argv[1]);
        log(1, "photo file path=", photoFileName.filePath());
    }
    else {
        QString dir;
        vector<QString> excludeList;
        getConfigPaths(dir, excludeList);
        QString firstImageFound=getFirstUntaggedImage(dir, excludeList);
        if (!firstImageFound.isEmpty()) {
            log(1, "no image file in cmd. line, first image found is ",
                firstImageFound);
            photoFileName.setFile(firstImageFound);
        }
    }
    PhotoCollectionWnd *w = new PhotoCollectionWnd();
    w->setPhotoFileName(photoFileName);
    w->exec();
}
