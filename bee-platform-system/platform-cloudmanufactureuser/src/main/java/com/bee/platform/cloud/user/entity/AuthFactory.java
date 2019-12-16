package com.bee.platform.cloud.user.entity;

import java.io.Serializable;

import com.baomidou.mybatisplus.enums.IdType;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.enums.IdType;
import com.baomidou.mybatisplus.activerecord.Model;
import java.io.Serializable;

/**
 * <p>
 * 
 * </p>
 *
 * @author zhigang.zhou123
 * @since 2019-09-19
 */
public class AuthFactory extends Model<AuthFactory> {

    private static final long serialVersionUID = 1L;

    /**
     * 工厂id
     */
    @TableId(value = "factory_id", type = IdType.AUTO)
    private Integer factoryId;
    /**
     * 工厂名称
     */
    private String factoryName;
    /**
     * 企业id
     */
    private Integer enterpriseId;


    public Integer getFactoryId() {
        return factoryId;
    }

    public void setFactoryId(Integer factoryId) {
        this.factoryId = factoryId;
    }

    public String getFactoryName() {
        return factoryName;
    }

    public void setFactoryName(String factoryName) {
        this.factoryName = factoryName;
    }

    public Integer getEnterpriseId() {
        return enterpriseId;
    }

    public void setEnterpriseId(Integer enterpriseId) {
        this.enterpriseId = enterpriseId;
    }

    @Override
    protected Serializable pkVal() {
        return this.factoryId;
    }

    @Override
    public String toString() {
        return "AuthFactory{" +
        ", factoryId=" + factoryId +
        ", factoryName=" + factoryName +
        ", enterpriseId=" + enterpriseId +
        "}";
    }
}
