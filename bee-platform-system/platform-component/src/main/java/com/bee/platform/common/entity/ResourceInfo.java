package com.bee.platform.common.entity;

import com.baomidou.mybatisplus.activerecord.Model;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.annotations.TableName;
import com.baomidou.mybatisplus.enums.IdType;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Date;

/**
 * @notes 资源
 * @Author junyang.li
 * @Date 17:23 2019/3/4
 **/
@Getter
@Setter
@ToString
@Accessors(chain = true)
@NoArgsConstructor
@TableName("t_resource")
public class ResourceInfo extends Model<ResourceInfo> {

    private static final long serialVersionUID = 1L;

    /**
     * 主键
     */
    @TableId(value = "id", type = IdType.AUTO)
    private Long id;
    /**
     * 资源id
     */
    private Long resourceId;
    /**
     * 资源名
     */
    private String resourceName;
    /**
     * 资源所属系统（）
     */
    private Integer sysType;
    /**
     * 资源url
     */
    private String resourceUrl;
    /**
     * 是否被拦截(0.访问放行，1.登录验证，2.权限验证)

     */
    private Integer intercept;
    /**
     * 上级资源id，没有则为0
     */
    private Long parentId;
    /**
     * 资源图标url
     */
    private String iconUrl;
    /**
     * 备注
     */
    private String notes;
    /**
     * 所在位置
     */
    private String position;
    /**
     * 是否有效，0无效，1有效
     */
    private Integer state;
    /**
     * 创建人   
     */
    private String creator;
    /**
     * 创建时间
     */
    private Date createTime;
    /**
     * 修改人
     */
    private String updateUserId;
    /**
     * 修改时间
     */
    private Date updateTime;


    @Override
    protected Serializable pkVal() {
        return this.id;
    }

}
