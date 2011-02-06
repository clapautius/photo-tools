#ifndef PHOTO_ARCHIVE_QT_H
#define PHOTO_ARCHIVE_QT_H

#include <QDialog>
#include <QLineEdit>
#include <QFileInfo>
#include <QListWidget>
#include <QPushButton>
#include <QLabel>
#include <QLayout>


class PhotoArchiveWnd : public QDialog
{
  Q_OBJECT

public:

    PhotoArchiveWnd( QWidget *parent = 0, char *name = 0 );

protected:

  
protected slots:

    void searchArchByName();

    void checkArchive();

    void itemClicked(QListWidgetItem *pItem);
    
private:

    void disableUserInteraction();

    void enableUserInteraction();

    void checkDuplicates(std::vector<QFileInfo> &);
    
    static QPixmap generatePreview(const QFileInfo &rFile);

    static bool uncompressToTmp(const QString &filePath, QString &tmpFileName);

    void writeToStatusBar(const QString text="");

    void updateDisplay();

    QHBoxLayout *mpButtonsLayout;
    
    QPushButton *mpCloseButton;
    QPushButton *mpCheckButton;

    QPushButton *mpSearchButton;
    
    QLineEdit *mpSubstringEdit;

    QListWidget *mpList;

    QLabel *mpStatusBar;
};


#endif
