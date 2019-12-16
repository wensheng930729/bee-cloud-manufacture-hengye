package com.bee.platform.cloud.si.manufacture.entity;

import java.io.Serializable;

import com.baomidou.mybatisplus.enums.IdType;
import java.math.BigDecimal;
import java.time.LocalDateTime;
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
 *  产成品待出库车辆信息
 * </p>
 *
 * @author fei.sun
 * @since 2019-09-25
 */
@Data
@EqualsAndHashCode(callSuper = true)
@Accessors(chain = true)
public class FinishedProductBeOutOfStorage extends Model<FinishedProductBeOutOfStorage> {

    private static final long serialVersionUID = 1L;

    /**
     * 自增主键
     */
    @TableId(value = "id", type = IdType.AUTO)
    private Long id;
    /**
     * 业务唯一标识id
     */
    private String contractCarId;
    /**
     * 工厂id
     */
    private Integer factoryId;
    /**
     * 当前登录的企业id
     */
    private Integer orgId;
    /**
     * 合同编号
     */
    private String contractId;
    /**
     * 承运商id
     */
    private Long carrierId;
    /**
     * 磅单id
     */
    private String  machineId;
    /**
     * 承运商
     */
    private String carrierName;
    /**
     * 车牌号
     */
    private String licensePlateNumber;
    /**
     * 货物id
     */
    private Integer productId;
    /**
     * 样品名称
     */
    private String productName;
    /**
     * 收货重量
     */
    private BigDecimal productNumber;
    /**
     * 货物数量单位
     */
    private String productUnit;
    /**
     * 司机名称
     */
    private String driverName;
    /**
     * 联系方式
     */
    private String contact;
    /**
     * 是否已出库（0未出库；1已出库）
     */
    private Integer outStorage;
    /**
     * 数据状态0删除1正常
     */
    private Integer status;
    /**
     * 创建人id
     */
    private Integer createId;
    /**
     * 创建人名称
     */
    private String creator;
    /**
     * 创建/申请时间
     */
    private LocalDateTime createTime;
    /**
     * 修改人id
     */
    private Integer modifyId;
    /**
     * 修改人
     */
    private String modifier;
    /**
     * 修改时间
     */
    private LocalDateTime modifyTime;


    @Override
    protected Serializable pkVal() {
        return this.id;
    }

}
