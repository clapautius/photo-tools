#ifndef PHOTO_ARCHIVE_QT_H
#define PHOTO_ARCHIVE_QT_H

#include <QDialog>
#include <QLineEdit>
#include <QFileInfo>
#include <QListWidget>

#define PHOTO_ARCHIVE_QT_VER "0"

class PhotoArchiveWnd : public QDialog
{
  Q_OBJECT

public:

    PhotoArchiveWnd( QWidget *parent = 0, char *name = 0 );

protected:

  
protected slots:

    void searchArchByName();

private:

    QLineEdit *mpSubstringEdit;

    QListWidget *mpList;
};


#endif
