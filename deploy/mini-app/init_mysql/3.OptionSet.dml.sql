-- Option Set: FieldType
-- Clean up historical data
DELETE FROM sys_option_set WHERE option_set_code='FieldType';
DELETE FROM sys_option_item WHERE option_set_code='FieldType';
-- Insert option set
INSERT INTO sys_option_set(option_set_code,name,description) VALUES('FieldType','Field Type','');
-- Insert option set items
INSERT INTO sys_option_item(option_set_code,item_code,item_name,sequence,parent_item_code,item_color,description) VALUES('FieldType','String','String',1,'','','');
INSERT INTO sys_option_item(option_set_code,item_code,item_name,sequence,parent_item_code,item_color,description) VALUES('FieldType','OneToOne','OneToOne',10,'','','');
INSERT INTO sys_option_item(option_set_code,item_code,item_name,sequence,parent_item_code,item_color,description) VALUES('FieldType','ManyToOne','ManyToOne',11,'','','');
INSERT INTO sys_option_item(option_set_code,item_code,item_name,sequence,parent_item_code,item_color,description) VALUES('FieldType','OneToMany','OneToMany',12,'','','');
INSERT INTO sys_option_item(option_set_code,item_code,item_name,sequence,parent_item_code,item_color,description) VALUES('FieldType','ManyToMany','ManyToMany',13,'','','');
INSERT INTO sys_option_item(option_set_code,item_code,item_name,sequence,parent_item_code,item_color,description) VALUES('FieldType','JSON','JSON',14,'','','');
INSERT INTO sys_option_item(option_set_code,item_code,item_name,sequence,parent_item_code,item_color,description) VALUES('FieldType','MultiString','MultiString',15,'','','');
INSERT INTO sys_option_item(option_set_code,item_code,item_name,sequence,parent_item_code,item_color,description) VALUES('FieldType','MultiOption','MultiOption',16,'','','');
INSERT INTO sys_option_item(option_set_code,item_code,item_name,sequence,parent_item_code,item_color,description) VALUES('FieldType','Filters','Filters',17,'','','');
INSERT INTO sys_option_item(option_set_code,item_code,item_name,sequence,parent_item_code,item_color,description) VALUES('FieldType','Orders','Orders',18,'','','');
INSERT INTO sys_option_item(option_set_code,item_code,item_name,sequence,parent_item_code,item_color,description) VALUES('FieldType','Integer','Integer',2,'','','');
INSERT INTO sys_option_item(option_set_code,item_code,item_name,sequence,parent_item_code,item_color,description) VALUES('FieldType','Long','Long',3,'','','');
INSERT INTO sys_option_item(option_set_code,item_code,item_name,sequence,parent_item_code,item_color,description) VALUES('FieldType','Double','Double',4,'','','');
INSERT INTO sys_option_item(option_set_code,item_code,item_name,sequence,parent_item_code,item_color,description) VALUES('FieldType','BigDecimal','BigDecimal',5,'','','');
INSERT INTO sys_option_item(option_set_code,item_code,item_name,sequence,parent_item_code,item_color,description) VALUES('FieldType','Option','Option Set',6,'','','');
INSERT INTO sys_option_item(option_set_code,item_code,item_name,sequence,parent_item_code,item_color,description) VALUES('FieldType','Boolean','Boolean',7,'','','');
INSERT INTO sys_option_item(option_set_code,item_code,item_name,sequence,parent_item_code,item_color,description) VALUES('FieldType','Date','Date',8,'','','');
INSERT INTO sys_option_item(option_set_code,item_code,item_name,sequence,parent_item_code,item_color,description) VALUES('FieldType','DateTime','DateTime',9,'','','');

-- Option Set: OptionItemColor
-- Clean up historical data
DELETE FROM sys_option_set WHERE option_set_code='OptionItemColor';
DELETE FROM sys_option_item WHERE option_set_code='OptionItemColor';
-- Insert option set
INSERT INTO sys_option_set(option_set_code,name,description) VALUES('OptionItemColor','Option Item Color','');
-- Insert option set items
INSERT INTO sys_option_item(option_set_code,item_code,item_name,sequence,parent_item_code,item_color,description) VALUES('OptionItemColor','LightGrey','Light Grey',1,'','','');
INSERT INTO sys_option_item(option_set_code,item_code,item_name,sequence,parent_item_code,item_color,description) VALUES('OptionItemColor','DarkRed','Dark Red',10,'','','');
INSERT INTO sys_option_item(option_set_code,item_code,item_name,sequence,parent_item_code,item_color,description) VALUES('OptionItemColor','DarkBlue','Dark Blue',11,'','','');
INSERT INTO sys_option_item(option_set_code,item_code,item_name,sequence,parent_item_code,item_color,description) VALUES('OptionItemColor','DarkPurple','Dark Purple',12,'','','');
INSERT INTO sys_option_item(option_set_code,item_code,item_name,sequence,parent_item_code,item_color,description) VALUES('OptionItemColor','LightGreen','Light Green',2,'','','');
INSERT INTO sys_option_item(option_set_code,item_code,item_name,sequence,parent_item_code,item_color,description) VALUES('OptionItemColor','LightYellow','Light Yellow',3,'','','');
INSERT INTO sys_option_item(option_set_code,item_code,item_name,sequence,parent_item_code,item_color,description) VALUES('OptionItemColor','LightRed','Light Red',4,'','','');
INSERT INTO sys_option_item(option_set_code,item_code,item_name,sequence,parent_item_code,item_color,description) VALUES('OptionItemColor','LightBlue','Light Blue',5,'','','');
INSERT INTO sys_option_item(option_set_code,item_code,item_name,sequence,parent_item_code,item_color,description) VALUES('OptionItemColor','LightPurple','Light Purple',6,'','','');
INSERT INTO sys_option_item(option_set_code,item_code,item_name,sequence,parent_item_code,item_color,description) VALUES('OptionItemColor','DarkGrey','Dark Grey',7,'','','');
INSERT INTO sys_option_item(option_set_code,item_code,item_name,sequence,parent_item_code,item_color,description) VALUES('OptionItemColor','DarkGreen','Dark Green',8,'','','');
INSERT INTO sys_option_item(option_set_code,item_code,item_name,sequence,parent_item_code,item_color,description) VALUES('OptionItemColor','DarkYellow','Dark Yellow',9,'','','');

-- Option Set: MaskingType
-- Clean up historical data
DELETE FROM sys_option_set WHERE option_set_code='MaskingType';
DELETE FROM sys_option_item WHERE option_set_code='MaskingType';
-- Insert option set
INSERT INTO sys_option_set(option_set_code,name,description) VALUES('MaskingType','Masking Type','');
-- Insert option set items
INSERT INTO sys_option_item(option_set_code,item_code,item_name,sequence,parent_item_code,item_color,description) VALUES('MaskingType','All','Masks All Content',1,'','','');
INSERT INTO sys_option_item(option_set_code,item_code,item_name,sequence,parent_item_code,item_color,description) VALUES('MaskingType','Name','Masks Name',2,'','','');
INSERT INTO sys_option_item(option_set_code,item_code,item_name,sequence,parent_item_code,item_color,description) VALUES('MaskingType','Email','Masks Email',3,'','','');
INSERT INTO sys_option_item(option_set_code,item_code,item_name,sequence,parent_item_code,item_color,description) VALUES('MaskingType','PhoneNumber','Masks Phone Number',4,'','','');
INSERT INTO sys_option_item(option_set_code,item_code,item_name,sequence,parent_item_code,item_color,description) VALUES('MaskingType','IdNumber','Masks ID Number',5,'','','');
INSERT INTO sys_option_item(option_set_code,item_code,item_name,sequence,parent_item_code,item_color,description) VALUES('MaskingType','CardNumber','Masks Card Number',6,'','','');

-- Option Set: IdStrategy
-- Clean up historical data
DELETE FROM sys_option_set WHERE option_set_code='IdStrategy';
DELETE FROM sys_option_item WHERE option_set_code='IdStrategy';
-- Insert option set
INSERT INTO sys_option_set(option_set_code,name,description) VALUES('IdStrategy','ID Strategy','');
-- Insert option set items
INSERT INTO sys_option_item(option_set_code,item_code,item_name,sequence,parent_item_code,item_color,description) VALUES('IdStrategy','DbAutoID','DB Auto-increment ID',1,'','','');
INSERT INTO sys_option_item(option_set_code,item_code,item_name,sequence,parent_item_code,item_color,description) VALUES('IdStrategy','ULID','ULID',2,'','','');
INSERT INTO sys_option_item(option_set_code,item_code,item_name,sequence,parent_item_code,item_color,description) VALUES('IdStrategy','TSIDLong','Long Time-Sorted ID',3,'','','');
INSERT INTO sys_option_item(option_set_code,item_code,item_name,sequence,parent_item_code,item_color,description) VALUES('IdStrategy','TSIDString','String Time-Sorted ID',4,'','','');
INSERT INTO sys_option_item(option_set_code,item_code,item_name,sequence,parent_item_code,item_color,description) VALUES('IdStrategy','SimpleID','Simple 16-digit Long ID',5,'','','');
INSERT INTO sys_option_item(option_set_code,item_code,item_name,sequence,parent_item_code,item_color,description) VALUES('IdStrategy','UUID','UUID',6,'','','');
INSERT INTO sys_option_item(option_set_code,item_code,item_name,sequence,parent_item_code,item_color,description) VALUES('IdStrategy','ExternalID','External ID',7,'','','');

-- Option Set: StorageType
-- Clean up historical data
DELETE FROM sys_option_set WHERE option_set_code='StorageType';
DELETE FROM sys_option_item WHERE option_set_code='StorageType';
-- Insert option set
INSERT INTO sys_option_set(option_set_code,name,description) VALUES('StorageType','Storage Type','');
-- Insert option set items
INSERT INTO sys_option_item(option_set_code,item_code,item_name,sequence,parent_item_code,item_color,description) VALUES('StorageType','RDBMS','RDBMS',1,'','','');
INSERT INTO sys_option_item(option_set_code,item_code,item_name,sequence,parent_item_code,item_color,description) VALUES('StorageType','ES','ElasticSearch',2,'','','');
INSERT INTO sys_option_item(option_set_code,item_code,item_name,sequence,parent_item_code,item_color,description) VALUES('StorageType','OLAP','OLAP Engine',3,'','','');

-- Option Set: BooleanValue
-- Clean up historical data
DELETE FROM sys_option_set WHERE option_set_code='BooleanValue';
DELETE FROM sys_option_item WHERE option_set_code='BooleanValue';
-- Insert option set
INSERT INTO sys_option_set(option_set_code,name,description) VALUES('BooleanValue','Boolean Option','');
-- Insert option set items
INSERT INTO sys_option_item(option_set_code,item_code,item_name,sequence,parent_item_code,item_color,description) VALUES('BooleanValue','true','Yes',1,'','','');
INSERT INTO sys_option_item(option_set_code,item_code,item_name,sequence,parent_item_code,item_color,description) VALUES('BooleanValue','false','No',2,'','','');

