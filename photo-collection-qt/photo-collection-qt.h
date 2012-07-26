#ifndef PHOTO_COLLECTION_QT_H
#define PHOTO_COLLECTION_QT_H

#include <map>
#include <vector>

#include <QDialog>
#include <QBoxLayout>
#include <QCheckBox>
#include <QLineEdit>
#include <QPushButton>
#include <QRadioButton>
#include <QTextEdit>
#include <QFileInfo>
#include <QCloseEvent>
#include <QLabel>

#define PHOTO_COLLECTION_QT_VER "1.4.1"

class PhotoCollectionWnd : public QDialog
{
  Q_OBJECT

public:
  PhotoCollectionWnd( QWidget *parent = 0, char *name = 0 );

  void setPhotoFileName(QFileInfo photoFileName);

  QString getUrl() const;
  QString getAuthor() const;
  QString getTitle() const;
  QString getType() const;
  QString getDetails() const;
  QString getExif() const;
  std::vector<QString> getAllTags() const;

protected:

  virtual void closeEvent(QCloseEvent * event);
  
protected slots:

  void updatePhotoXml();

  void previewPhotoXml();

  void lookForImage();

private:

  void resetFields();

  void setFileNameField(const QFileInfo &file);
  
  int mTagLayoutPos;

#define TAGS_NO_COLUMNS 6
  std::vector<QVBoxLayout*> mTagsLayouts;

  QVBoxLayout *pTagsLayout1;
  QVBoxLayout *pTagsLayout2;
  QVBoxLayout *pTagsLayout3;
  QVBoxLayout *pTagsLayout4;
  QVBoxLayout *pTagsLayout5;

  QLabel *mpNameField; // read-only
  QLineEdit *mpUrlEdit;
  QLineEdit *mpAuthorEdit;
  QLineEdit *mpTitleEdit;
  QRadioButton *mpAmateurBtn, *mpEstablishedBtn, *mpPaintingBtn;
  QLineEdit *mpOtherTagsEdit;
  QTextEdit *mpExifEdit;
  QTextEdit *mpDetailsEdit;
  
  QFileInfo mPhotoFileName;

  void addTagCheckbox(const char* checkboxText);

  std::map<QString, QCheckBox*> mStandardTags;

  QString generateXmlText() const;

  bool mSaved;
};


class PhotoColPreviewWnd : public QDialog
{
  Q_OBJECT

public:
    PhotoColPreviewWnd(QString xmlText );

private:

    QString mXmlText;
};

#endif
