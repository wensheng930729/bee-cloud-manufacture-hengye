package com.bee.platform.cloud.si.manufacture.entity;

import java.io.Serializable;

import com.baomidou.mybatisplus.enums.IdType;

import java.math.BigDecimal;
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
 * 矿热炉记录明细表
 * </p>
 *
 * @author huangxin123
 * @since 2019-09-26
 */
@Data
@EqualsAndHashCode(callSuper = true)
@Accessors(chain = true)
public class ProOreFurnaceRecordDetail extends Model<ProOreFurnaceRecordDetail> {

    private static final long serialVersionUID = 1L;

    @TableId(value = "id", type = IdType.AUTO)
    private Long id;
    /**
     * 矿热炉记录主表id
     */
    private Long oreRecordId;
    /**
     * 创建时间
     */
    private Date createTime;
    /**
     * 更新时间
     */
    private Date modifyTime;
    /**
     * 捣炉记录：json数据
     */
    private String stirFurnaceRecord;
    /**
     * 电极：json数据
     */
    private String electrode;
    /**
     * 出炉记录：json数据
     */
    private String outFurnaceRecord;
    /**
     * 配电记录：json数据
     */
    private String powerRecord;
    /**
     * 数据状态:0删除1正常
     */
    private Integer status;
    /**
     * 电力消耗
     */
    private BigDecimal powerConsume;


    @Override
    protected Serializable pkVal() {
        return this.id;
    }

}
