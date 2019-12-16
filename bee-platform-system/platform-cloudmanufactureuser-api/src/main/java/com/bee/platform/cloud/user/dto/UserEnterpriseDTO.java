package com.bee.platform.cloud.user.dto;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * @description: 用户企业关系
 * @author: junyang.li
 * @create: 2019-09-19 11:02
 **/
@Getter
@Setter
@ToString
@Accessors(chain = true)
@NoArgsConstructor
public class UserEnterpriseDTO implements Serializable {

    private static final long serialVersionUID = -4520142415711184363L;
    /**
     * 用户id
     */
    private Integer userId;
    /**
     * 企业id
     */
    private Integer enterpriseId;
    /**
     * 企业名称
     */
    private String enterpriseName;
    /**
     * 工厂id
     */
    private Integer factoryId;
    /**
     * 工厂id
     */
    private String factoryName;
    /**
     * 状态：1启动 0禁用
     */
    private Integer status;
}
