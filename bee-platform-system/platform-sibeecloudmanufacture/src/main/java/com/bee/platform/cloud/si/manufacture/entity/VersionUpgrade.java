package com.bee.platform.cloud.si.manufacture.entity;

import java.io.Serializable;

import com.baomidou.mybatisplus.enums.IdType;
import java.util.Date;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.enums.IdType;
import com.baomidou.mybatisplus.activerecord.Model;
import java.io.Serializable;

import com.baomidou.mybatisplus.annotations.Version;

import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.experimental.Accessors;

/**
 * <p>
 * 版本升级描述
 * </p>
 *
 * @author LL123
 * @since 2019-11-25
 */
@Data
@EqualsAndHashCode(callSuper = true)
@Accessors(chain = true)
public class VersionUpgrade extends Model<VersionUpgrade> {

    private static final long serialVersionUID = 1L;

    /**
     * 主键
     */
    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;
    /**
     * 版本号
     */
    private String versionNum;
    /**
     * 描述

     */
    private String description;
    /**
     * 升级时间
     */
    private Date upgradeTime;


    @Override
    protected Serializable pkVal() {
        return this.id;
    }

}
