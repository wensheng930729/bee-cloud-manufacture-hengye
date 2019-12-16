package com.bee.platform.cloud.si.manufacture.entity;

import com.baomidou.mybatisplus.activerecord.Model;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.enums.IdType;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Date;

/**
 * <p>
 * 地磅称重数据
 * </p>
 *
 * @author chenxm66777123
 * @since 2019-09-26
 */
@Data
@EqualsAndHashCode(callSuper = true)
@Accessors(chain = true)
public class WeightMachineData extends Model<WeightMachineData> {

    private static final long serialVersionUID = 1L;

    @TableId(value = "id", type = IdType.AUTO)
    private Long id;
    /**
     * 设备id
     */
    private String deviceId;
    /**
     * 称重数据
     */
    private String weightData;
    /**
     * 称重时间
     */
    private Date createTime;


    @Override
    protected Serializable pkVal() {
        return this.id;
    }

}
