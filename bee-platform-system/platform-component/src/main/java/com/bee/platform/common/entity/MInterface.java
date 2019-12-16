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
 * @notes 接口
 * @Author junyang.li
 * @Date 10:13 2019/4/28
 **/
@Getter
@Setter
@ToString
@NoArgsConstructor
@Accessors(chain=true)
@TableName("m_interface")
public class MInterface extends Model<MInterface> {

    private static final long serialVersionUID = 1L;

    /**
     * 主键
     */
    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;
    /**
     * 接口id
     */
    private Integer interfaceId;
    /**
     * 接口名称
     */
    private String interfaceName;
    /**
     * 接口uri
     */
    private String interfaceUri;
    /**
     * 接口类型
     */
    private Integer interfaceType;
    /**
     * 接口说明
     */
    private String explain;
    /**
     * 是否有效0无效，1有效
     */
    private Integer status;
    /**
     * 创建人id
     */
    private Long createId;
    /**
     * 创建人姓名
     */
    private String creator;
    /**
     * 创建时间
     */
    private Date createTime;
    /**
     * 修改人id
     */
    private Long modifyId;
    /**
     * 修改人姓名
     */
    private String modifier;
    /**
     * 修改时间
     */
    private Date modifyTime;


    @Override
    protected Serializable pkVal() {
        return this.id;
    }
}
