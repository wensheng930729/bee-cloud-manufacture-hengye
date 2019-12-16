package com.bee.platform.cloud.user.entity;

import java.io.Serializable;

import com.baomidou.mybatisplus.annotations.TableName;
import com.baomidou.mybatisplus.enums.IdType;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.enums.IdType;
import com.baomidou.mybatisplus.activerecord.Model;
import lombok.*;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * <p>
 * 
 * </p>
 *
 * @author junyang.li123
 * @since 2019-10-12
 */
@Getter
@Setter
@ToString
@NoArgsConstructor
@Accessors(chain=true)
@TableName("auth_app_version")
public class AuthAppVersion extends Model<AuthAppVersion> {

    private static final long serialVersionUID = 1L;

    /**
     * 主键
     */
    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;
    /**
     * app类型
     */
    private String appType;
    /**
     * app最新版本下载地址
     */
    private String downloadAddr;
    /**
     * 最新版本号
     */
    private String latestVersion;
    /**
     * 备用最新下载地址
     */
    private String spareAddr;

    @Override
    protected Serializable pkVal() {
        return this.id;
    }
}
