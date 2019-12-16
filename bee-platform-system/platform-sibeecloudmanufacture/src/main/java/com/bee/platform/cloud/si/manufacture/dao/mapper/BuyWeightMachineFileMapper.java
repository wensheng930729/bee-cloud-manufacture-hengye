package com.bee.platform.cloud.si.manufacture.dao.mapper;

import com.baomidou.mybatisplus.mapper.BaseMapper;
import com.bee.platform.cloud.si.manufacture.entity.BuyWeightMachineFile;

import java.util.List;


/**
 * @Description 磅单附件信息 Mapper 接口
 * @author chenxm66777123
 * @Date 2019/9/23 13:50
 * @version 1.0.0
 */
public interface BuyWeightMachineFileMapper extends BaseMapper<BuyWeightMachineFile> {

    /**
     * @Description 批量保存附件信息
     * @author chenxm66777123
     * @Date 2019/9/24 14:35
     * @version 1.0.0
     */
    int batchSaveFile(List<BuyWeightMachineFile> buyWeightMachineFileList);

    /**
     * @Description 六个小时执行一次删除数据任务
     * @author chenxm66777123
     * @Date 2019/10/21 9:23
     * @version 1.0.0
     */
    void clearWeightData();
}
