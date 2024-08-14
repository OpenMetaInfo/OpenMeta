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

    /**
     * Uses the Quartz cron expression, consisting of 7 fields in the order:
     * [seconds] [minutes] [hours] [day] [month] [week] [year (optional)]
     */
    public static final CronType CRON_FORMAT = CronType.QUARTZ;

    private static final CronParser cronParser = new CronParser(CronDefinitionBuilder.instanceDefinitionFor(CRON_FORMAT));
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

    /**
     * Get the Cron object by the cron expression.
     *
     * @param cronName Cron name
     * @param cronExpression Cron expression
     * @return Cron object
     */
    public static Cron getCron(String cronName, String cronExpression) {
        try {
            Cron cron = cronParser.parse(cronExpression);
            cron.validate();
            return cron;
        } catch (java.lang.IllegalArgumentException e) {
            throw new IllegalArgumentException("The cron expression of {0}({1}) is invalid: {2}",
                    cronName, cronExpression, e.getMessage());
        }
    }
}
