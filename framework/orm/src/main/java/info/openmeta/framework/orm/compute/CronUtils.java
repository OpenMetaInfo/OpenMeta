package info.openmeta.framework.orm.compute;

import com.cronutils.descriptor.CronDescriptor;
import com.cronutils.model.Cron;
import com.cronutils.model.CronType;
import com.cronutils.model.definition.CronDefinitionBuilder;
import com.cronutils.parser.CronParser;
import info.openmeta.framework.base.constant.BaseConstant;
import info.openmeta.framework.base.exception.IllegalArgumentException;

/**
 * Expression calculation tool class.
 * Definition of static methods, and imported to calculation engine.
 */
public class CronUtils {
    private static final CronParser cronParser = new CronParser(CronDefinitionBuilder.instanceDefinitionFor(CronType.QUARTZ));
    private static final CronDescriptor descriptor = CronDescriptor.instance(BaseConstant.DEFAULT_LANGUAGE);

    /**
     * Validate the cron expression.
     * @param cronExpression cron expression
     * @return true if the cron expression is valid
     */
    public static boolean cronValidator(String cronExpression) {
        Cron cron = cronParser.parse(cronExpression);
        try {
            cron.validate();
            return true;
        } catch (Exception e) {
            return false;
        }
    }

    /**
     * Get the semantic description of the cron expression.
     * @param cronExpression cron expression
     * @return semantic description
     */
    public static String cronSemantic(String cronExpression) {
        Cron cron = cronParser.parse(cronExpression);
        try {
            cron.validate();
        } catch (Exception e) {
            throw new IllegalArgumentException("Cron expression {0} is invalid: {1}", cronExpression, e.getMessage());
        }
        return descriptor.describe(cron);
    }

}
