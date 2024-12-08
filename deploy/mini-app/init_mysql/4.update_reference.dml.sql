-- Update the modelId field in the sys_field table.
UPDATE sys_field sf JOIN sys_model sm ON sf.model_name = sm.model_name SET sf.model_id = sm.id;

-- Update the optionSetId field in the sys_option_item table.
UPDATE sys_option_item soi JOIN sys_option_set sos ON soi.option_set_code = sos.option_set_code SET soi.option_set_id = sos.id;