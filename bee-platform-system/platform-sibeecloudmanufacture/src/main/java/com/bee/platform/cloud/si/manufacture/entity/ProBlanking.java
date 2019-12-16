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
 * 下料表
 * </p>
 *
 * @author huangxin123
 * @since 2019-09-25
 */
@Data
@EqualsAndHashCode(callSuper = true)
@Accessors(chain = true)
public class ProBlanking extends Model<ProBlanking> {

    private static final long serialVersionUID = 1L;

    /**
     * 下料id
     */
    @TableId(value = "id", type = IdType.AUTO)
    private Long id;
    /**
     * 配料id
     */
    private Long ingredientId;
    /**
     * 料批id
     */
    private Long batchId;
    /**
     * 成品id
     */
    private Integer finishProductId;
    /**
     * 料批名称
     */
    private String materialName;
    /**
     * plc设备名称
     */
    private String plcName;
    /**
     * 炉号id
     */
    private Integer furnaceId;
    /**
     * 炉号名称
     */
    private String furnaceName;
    /**
     * 班次:1一班，2二班，3三班
     */
    private Integer shift;
    /**
     * 数量
     */
    private BigDecimal num;
    /**
     * 主料数量
     */
    private BigDecimal mainNum;
    /**
     * 单位
     */
    private String unit;
    /**
     * 下料时间
     */
    private Date blankingTime;
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
     * 工厂id
     */
    private Integer factoryId;
    /**
     * 企业id
     */
    private Integer companyId;
    /**
     * 修改人id
     */
    private Integer modifyId;
    /**
     * 数据状态:0删除1正常
     */
    private Integer status;


    @Override
    protected Serializable pkVal() {
        return this.id;
    }

}
