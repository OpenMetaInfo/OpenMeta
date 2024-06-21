package info.openmeta.framework.web.constant;

public class WebConstant {

    public static class Header {
        public final static String H5_HOST = "x-h5-host";
    }

    public static class Api {
        /** The API endpoint to obtain the Access Token in OAuth2, default is '/oauth2/token' */
        public final static String OAUTH2_TOKEN = "/oauth2/token";
        /** The API endpoint to upgrade the runtime using OAuth2 */
        public final static String OAUTH2_UPGRADE_API = "/oauth2/upgrade/releasePackage";
    }

}
