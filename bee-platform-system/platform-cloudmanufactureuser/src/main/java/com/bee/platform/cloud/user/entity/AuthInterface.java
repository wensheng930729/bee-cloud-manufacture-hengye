package com.bee.platform.cloud.user.entity;

import com.baomidou.mybatisplus.activerecord.Model;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.annotations.TableName;
import com.baomidou.mybatisplus.enums.IdType;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Date;

/**
 * <p>
 *  接口
 * </p>
 *
 * @author xin.huang
 * @since 2019-05-20
 */
@Data
@NoArgsConstructor
@Accessors(chain=true)
@TableName("auth_interface")
public class AuthInterface extends Model<AuthInterface> {

    private static final long serialVersionUID = 1L;

    /**
     * 资源id
     */
    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;
    /**
     * 资源名称
     */
    private String name;
    /**
     * 资源类型
     */
    private String type;
    /**
     * 子系统标识
     */
    private String subSys;
    /**
     * 排序
     */
    private Integer orderNum;

    /**
     * 资源地址
     */
    private String url;
    
    /**
     * 接口路由
     */
    private String beeRouter;
    
    /**
     * 是否启用：1启用 0禁用 
     */
    private Integer status;
    /**
     * 创建时间
     */
    private Date createTime;
    /**
     * 更新时间
     */
    private Date updateTime;
    /**
     * 是否删除：1是 0否
     */
    private Integer deleted;

    @Override
    protected Serializable pkVal() {
        return this.id;
    }

}
