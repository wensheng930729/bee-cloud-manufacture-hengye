 package com.bee.platform.common.config.property;

import java.util.Optional;

import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.stereotype.Component;

import lombok.Getter;
import lombok.Setter;

 /**
 * @author Raphael.dq
 * @date 2019/05/21
 */
@Getter
@Setter
@Component
@ConfigurationProperties(prefix = "bee.common.auth")
 public class AuthConfigProperties {
     
     /**
      * 不拦截的uri pattern
      */
     private String excludes;
     
     /**
      * 子系统标识: 供应链金融-bee_supply_chain_finance, 平台-bee_platform,
      * 蜂创物联-bee_iot,集蜂联运-bee_logistics,线上蜂贸-bee_trade,金蜜ERP-bee_erp,
      * 具体值请参考码表t_system_code_t中sys_group_id为sub_system的sys_code值
      */
     private String platform;
     
     /**
     * 开关: true开启权限拦截，false-关闭
     */
    private Boolean switchOn;
    
    /**
     * 配置平台权限地址
     */
    private String address;
    
    
    public String getExpectedAuthAddress() {
        return Optional.ofNullable(address).map(s -> s.endsWith("/") ? s : s + "/").orElse("");
    }
    
 }