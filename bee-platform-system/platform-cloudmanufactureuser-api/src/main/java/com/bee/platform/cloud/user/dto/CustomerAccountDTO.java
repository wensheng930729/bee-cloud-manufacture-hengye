package com.bee.platform.cloud.user.dto;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * @description: 客户账号
 * @author: junyang.li
 * @create: 2019-10-21 19:31
 **/
@Getter
@Setter
@NoArgsConstructor
@Accessors(chain = true)
@ToString
public class CustomerAccountDTO implements Serializable {

    private static final long serialVersionUID = 1191706518229045446L;
    /**
     * 账号
     */
    private String username;
    /**
     * 用户名称
     */
    private String name;
    /**
     * 所属企业
     */
    private Integer enterpriseId;
    /**
     * 是否为承运商（0否 1是）
     */
    private Integer carrier;
}
