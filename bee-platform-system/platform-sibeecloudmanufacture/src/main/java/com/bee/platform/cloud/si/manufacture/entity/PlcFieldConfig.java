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
 * plc字段相关的配置
 * </p>
 *
 * @author MP123
 * @since 2019-10-11
 */
@Data
@EqualsAndHashCode(callSuper = true)
@Accessors(chain = true)
public class PlcFieldConfig extends Model<PlcFieldConfig> {

    private static final long serialVersionUID = 1L;

    /**
     * 主键
     */
    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;
    /**
     * 字段英文名
     */
    private String field;
    /**
     * 字段中文名
     */
    private String fieldName;
    /**
     * 工厂id
     */
    private Integer factoryId;
    /**
     * plc 的id
     */
    private Integer plcId;
    /**
     * PLC 漏斗中用户标识字段的类型  批次字段和漏斗字段
     */
    private Integer fieldType;
    /**
     * 单位
     */
    private String unit;
    /**
     * 是否有效 0无效 ，1 有效
     */
    private Integer status;
    /**
     * 是否删除 0否 ，1 删除
     */
    private Integer deleted;
    /**
     * 创建人id
     */
    private Integer createId;
    /**
     * 创建人名称
     */
    private String creator;
    /**
     * 创建时间
     */
    private Date createTime;
    /**
     * 修改人id
     */
    private Integer modifyId;
    /**
     * 修改人名称
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
