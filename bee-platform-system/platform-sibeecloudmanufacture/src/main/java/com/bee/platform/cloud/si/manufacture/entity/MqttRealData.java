package com.bee.platform.cloud.si.manufacture.entity;

import java.io.Serializable;

import com.baomidou.mybatisplus.enums.IdType;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.enums.IdType;
import com.baomidou.mybatisplus.activerecord.Model;
import java.io.Serializable;
import java.util.Date;

import com.baomidou.mybatisplus.annotations.Version;

import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.experimental.Accessors;

/**
 * <p>
 * 
 * </p>
 *
 * @author MP123
 * @since 2019-10-17
 */
@Data
@EqualsAndHashCode(callSuper = true)
@Accessors(chain = true)
public class MqttRealData extends Model<MqttRealData> {

    private static final long serialVersionUID = 1L;

    /**
     * 主键
     */
    @TableId(value = "id", type = IdType.AUTO)
    private Long id;
    /**
     * MQTT 中是实时数据
     */
    private String data;
    /**
     * 时间
     */
    private Date time;


    @Override
    protected Serializable pkVal() {
        return this.id;
    }

}
