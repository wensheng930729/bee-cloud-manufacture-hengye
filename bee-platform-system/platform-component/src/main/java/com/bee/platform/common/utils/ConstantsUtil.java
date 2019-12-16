package com.bee.platform.common.utils;

/**
 * notes
 * author junyyang.li
 * create 2018/11/6 0006 10:11
 **/
public class ConstantsUtil {

    public final static String COLON = ":";


    public final static String COMMA = ",";
    public final static String SLASH = "/";
    public final static String UNDERLINE = "_";
    public final static String LINE = "-";
    public final static String PERCENT = "%";
    public final static String DOUBLE_PERCENT = "%%";
    public final static String STAR = "*";
    public final static String QUESTION="?";
    public final static String EQUAL="=";
    public static final Integer FRIST = 1;
    public static final int MAX_SIZE = 5000;


    /** 15分钟*/
    public static final int SECOND=900;
    /** 5分钟*/
    public static final int FIFTEEN_MINUTE=900;
    /** 2小时*/
    public static final int TWO_HOURS=7200;

    /** 信息保存时间单位秒 */
    public static final int OVERDUE=86400;
    public static final long ONE_DAY=86400L;
    /**权限拦截开关，0为false ，1为true*/
    public static final int ZERO = 0;
    public static final String MINUS="-1";


    /**系统标识*/
    public final static String OPTIONS="OPTIONS";
    public final static String SYS_TOKEN="sysToken";
    public static final String CLOUD_MAF_TYPE="cloudMafType";
    public static final String CMF="cmf_";
    public final static String OK="OK";
    //内部服务请求的标识
    public static final String INNER_CLIENT_ID="innerClientId";
    public static final String INNER_SIMANUFACTURE="platform-sibeecloudmanufacture";
    public static final String INNER_USERMANUFACTURE="platform-cloudmanufactureuser";
    /** 盘点单开头标识*/
    public static final String INVENTORY_ORDER="PD";

    /** redis键*/
    public static final String VALIDATE_CODE="hengye_validate_code_";
    public static final String VALIDATE_RESULT="hengye_validate_result_";
    public static final String ALL_TEST_ACCOUNT_KEY="hengye_test_account_all";
    public static final String ALL_REGION_KEY="hengye_platform_all_region";
    public static final String ALL_REGION_TREE_NODE_KEY="hengye_all_region_tree_node";
    public static final String LOGIN_COUNT_KEY="hengye_login_count_key_";
    public static final String ALL_FIELD_CONFIG_KEY="hengye_all_field_key";
    public static final String USER_TOKEN_GROUP="hengye_user_token_group_";


    /**管理后台的redis键 */
    public static final String ALL_NOTICE_TEMPLATE="hengye_all_notice_template";
    /** 角色对应的资源**/
    public static final String CMF_RESOURCE_ROLE_HASH_KEY="hengye_cmf_resource_role_hash_";
    /** 配置是否走缓存的全局开关：1走缓存,0不走缓存 */
    public static final String CMF_CONFIG_SWITCH = "hengye_cmf_config_switch";
    /** app应用的所有资源的缓存键**/
    public static final String CMF_APP_RESOURCEID="hengye_cmf_app_resourceId";
    /** web应用的所有资源的缓存键**/
    public static final String CMF_WEB_RESOURCEID="hengye_cmf_web_resourceId";



    /** 码表组*/
    public static final String TEST_ACCOUNT="test_account";



    /** 配置表*/
    /** 权限认证开关*/
    public static final String TOKEN_EXPIRES_IN="platform_token_expires_in";
    /** 配置表中是否允许测试账号登陆的开关键*/
    public static final String TEST_ACCOUNT_LOGIN="test_account_login";
    /** 配置表中app最新的版本号*/
    public static final String APP_LATEST_VERSION="app_latest_version";
    /** 配置表中短信验证码的失效时间*/
    public static final String SMS_CODE_EXP_TIME="sms_code_exp_time";
    /** 邮件 */
    public static final String V_EMAIL="^\\w+((-\\w+)|(\\.\\w+))*\\@[A-Za-z0-9]+((\\.|-)[A-Za-z0-9]+)*\\.[A-Za-z0-9]+$";


    /** 华为obs对象存储返回参数*/
    public final static String OBJECTURL = "objectUrl";
    /** 华为obs对象存储返回参数*/
    public final static String ETAG = "etag";
    /** 图片类型*/
    public static final String[] UPLOAD_TYPE=new String[]{"0","1","2"};
    /** 图片类型*/
    public static final String[] IMAGE_FORMAT=new String[]{".png",".PNG",".jpg",".JPG",".jpeg",".JPEG"};
    /** office类型*/
    public static final String[] OFFICE_FORMAT=new String[]{".xls",".XLS",".xlsx",".XLSX",".doc",".DOC",".docx",".DOCX"};
    /** office类型*/
    public static final String[] OFFICE_PDF_FORMAT=new String[]{".pdf",".PDF",".doc",".DOC",".docx",".DOCX"};

}
