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
 * 配料主表
 * </p>
 *
 * @author huangxin123
 * @since 2019-09-25
 */
@Data
@EqualsAndHashCode(callSuper = true)
@Accessors(chain = true)
public class ProIngredient extends Model<ProIngredient> {

    private static final long serialVersionUID = 1L;

    /**
     * 配料主表id
     */
    @TableId(value = "id", type = IdType.AUTO)
    private Long id;
    /**
     * 料批id
     */
    private Long batchId;
    /**
     * 料批名称
     */
    private String materialName;
    /**
     * plc设备名称
     */
    private String plcName;
    /**
     * plc设备id
     */
    private Integer plcId;
    /**
     * 创建时间
     */
    private Date createTime;
    /**
     * 更新时间
     */
    private Date modifyTime;
    /**
     * 创建人id
     */
    private Integer createId;
    /**
     * 修改人id
     */
    private Integer modifyId;
    /**
     * 工厂id
     */
    private Integer factoryId;
    /**
     * 企业id
     */
    private Integer companyId;
    /**
     * 是否PLC设备配料：0否，1是
     */
    private Integer type;
    /**
     * 下料状态，1已下料，0未下料
     */
    private Integer blankingStatus;
    /**
     * 数据状态:0删除1正常
     */
    private Integer status;


    @Override
    protected Serializable pkVal() {
        return this.id;
    }

}
