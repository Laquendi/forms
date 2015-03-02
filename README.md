Files that are interesting:

* Database access options: config/mysql.yml
* Database table definitions: config/models
* Form definitions: Forms.hs
* Localization data: messages/*.msg

Notes on localization:
* English is the default language:
All used strings must be defined in messages/en.msg but others are optional.
* When you use localized string in code it begins with Msg (like UserAdded -> MsgUserAdded)
