package com.bee.platform.common.constants.enums;

import lombok.Getter;
import org.springframework.util.StringUtils;

/**
 * @description: 平台类型
 * @author: junyang.li
 * @create: 2019-09-19 09:52
 **/
@Getter
public enum PlatformType {

    /**
     * app
     */
    CLOUD_MAF_APP("cloud_maf_app","appToken"),
    /**
     * web
     */
    CLOUD_MAF_WEB("cloud_maf_web","webToken"),
    /**
     * bi
     */
    CLOUD_MAF_BI("cloud_maf_bi","biToken"),
    ;

    private String value;

    private String token;

    PlatformType(String value, String token) {
        this.value = value;
        this.token = token;
    }

    /**
     * @notes: 判断类型是否正确
     * @Author: junyang.li
     * @Date: 9:59 2019/9/25
     * @param value :
     * @return: com.bee.platform.common.constants.enums.PlatformType
     */
    public static boolean getType(String value){
        if(!StringUtils.isEmpty(value)) {
            return CLOUD_MAF_APP.getValue().equals(value)
                    || CLOUD_MAF_WEB.getValue().equals(value);
        }
        return false;
    }
}
