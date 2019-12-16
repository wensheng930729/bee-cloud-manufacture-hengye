import { message, Divider } from 'antd';
import React, { useState, useEffect } from 'react';
import { connect } from 'dva';
import moment from 'moment';
import Link from 'umi/link';
import {
  FormItemInputSearch,
  FormItemRangePicker,
  FormItemInput,
  FormItemSelect,
  TableWrapper,
  CustomModalHOC,
  CustomFormHOC,
} from '@/components/FormWidget';

import {
  purchaseStorageWaitService,
  purchaseStorageService,
} from '../services/purchaseStorageService';

import styles from './index.less';

const startTime = moment()
  .subtract(30, 'days')
  .format('YYYY-MM-DD');

const endTime = moment().format('YYYY-MM-DD');

// 顶部表单查询组件
const MyFormSearch = props => {
  const {
    form: { getFieldDecorator },
  } = props;
  const formItemLayout = {};

  return (
    <>
      <FormItemInputSearch
        getFieldDecorator={getFieldDecorator}
        label=""
        fieldId="contractNum"
        required={false}
        formItemLayout={formItemLayout}
        inputProps={{ placeholder: '按采购合同号查询' }}
      />
      <FormItemInputSearch
        getFieldDecorator={getFieldDecorator}
        label=""
        fieldId="supplierName"
        required={false}
        formItemLayout={formItemLayout}
        inputProps={{ placeholder: '按供应商查询' }}
      />

      <FormItemRangePicker
        getFieldDecorator={getFieldDecorator}
        label="创建日期"
        fieldId="dateRange"
        required={false}
        datePickerProps={{ format: 'YYYY-MM-DD' }}
        formItemLayout={formItemLayout}
      />
    </>
  );
};

/*
新增入库的modal弹窗
*/
const ComfirmForm = props => {
  const {
    form: { getFieldDecorator },
    storageOptions,
    productsOptions,
    productSpecOptions,
  } = props;
  const formItemLayout = {
    labelCol: { span: 4 },
    wrapperCol: { span: 16 },
  };

  return (
    <>
      <FormItemSelect
        getFieldDecorator={getFieldDecorator}
        label="仓库"
        fieldId="storageId"
        required
        selectProps={{ options: storageOptions }}
        formItemLayout={formItemLayout}
      />
      <FormItemSelect
        getFieldDecorator={getFieldDecorator}
        label="产品"
        fieldId="productId"
        required
        selectProps={{ options: productsOptions }}
        formItemLayout={formItemLayout}
      />
      <FormItemSelect
        getFieldDecorator={getFieldDecorator}
        label="规格"
        fieldId="productSpecId"
        required
        selectProps={{ options: productSpecOptions }}
        formItemLayout={formItemLayout}
      />
      <FormItemInput
        getFieldDecorator={getFieldDecorator}
        label="入库数量"
        fieldId="productNumber"
        required
        formItemLayout={formItemLayout}
      />
    </>
  );
};
// 创建表单
const NewComfirmForm = CustomFormHOC(ComfirmForm);
// 创建modal
const NewComfirmFormModal = CustomModalHOC(NewComfirmForm);

/*
查看新增入库详情
*/
const CheckViewComfirm = props => {
  const { detailDataWithAdd } = props;
  return (
    <div>
      {detailDataWithAdd.map(item => (
        <div>
          <div style={{ marginLeft: '40%' }}>
            <p>
              <span className={styles.label}> 产品名称:</span>
              {item.productName}
            </p>
            <p>
              <span className={styles.label}> 产品数量:</span>
              {item.productNumber}
            </p>
            <p>
              <span className={styles.label}> 产品规格:</span>
              {item.productSpecName}
            </p>
            <p>
              <span className={styles.label}> 仓库:</span>
              {item.storageName}
            </p>
            <p>
              <span className={styles.label}> 时间:</span>
              {item.time}
            </p>
          </div>
          <Divider />
        </div>
      ))}
    </div>
  );
};
// 创建modal
const CheckViewComfirmModal = CustomModalHOC(CheckViewComfirm);

const Index = props => {
  const {
    dispatch,
    storageOptions,
    productsOptions,
    productSpecOptions,
    saveFreeStorageParams,
    detailDataWithAdd,
  } = props;
  // 前端默认对 日期列降序排列
  const [sortOrder, setSortOrder] = useState('descend');
  // 当前TAB栏目
  const [tabKey, setTabKey] = useState('1');
  // "新增入库"弹窗是否可见
  const [confirmModalVisible, setConfirmModalVisible] = useState(false);

  // "查看新增入库"弹窗是否可见
  const [checkViewConfirmModalVisible, setCheckViewConfirmModalVisible] = useState(false);

  // 这里用于请求 顶部搜索表单中的下拉框数据
  useEffect(() => {
    // 这里在mount时执行一次
    dispatch({
      type: 'purchaseWarehousingModel/fetchInitOptionsEffect',
      payload: {},
      callback: (_, status, msg) => {
        if (status === 'error') {
          message.error(msg);
        }
      },
    });
  }, []);

  const otherColumnsForWait = [
    {
      title: '待入库数量',
      dataIndex: 'productNumber',
    },
    {
      title: '到厂日期',
      dataIndex: 'arrivalTime',
      // sortOrder,
      // sorter: (a, b) => {
      //   const yearA = a.arrivalTime.slice(0, 4);
      //   const monthA = a.arrivalTime.slice(5, 7);
      //   const dayA = a.arrivalTime.slice(8, 10);

      //   const newAStr = `${yearA}-${monthA}-${dayA}`;

      //   const yearB = b.arrivalTime.slice(0, 4);
      //   const monthB = b.arrivalTime.slice(5, 7);
      //   const dayB = b.arrivalTime.slice(8, 10);

      //   const newBStr = `${yearB}-${monthB}-${dayB}`;

      //   const bool = moment(newAStr).isBefore(newBStr);

      //   return bool;
      // },
    },
  ];

  const otherColumns = [
    {
      title: '入库日期',
      dataIndex: 'storageTime',
    },
    {
      title: '入库数量',
      dataIndex: 'actualProductNumber',
    },
  ];

  const columns = [
    {
      title: '对应合同号',
      dataIndex: 'contractId',
    },
    {
      title: '供应商',
      dataIndex: 'supplierName',
    },
    {
      title: '对应车号',
      dataIndex: 'licensePlateNumber',
    },

    {
      title: '操作',
      dataIndex: 'operation',

      render: (text, record) => (
        <Link
          to={{
            pathname: '/inventoryManage/purchaseWarehousing/detail',
            query: { buyProductPendingStorageId: record.buyProductPendingStorageId },
          }}
        >
          查看详情
        </Link>
      ),
    },
  ];

  if (tabKey === '1') {
    columns.splice(3, 0, ...otherColumnsForWait);
  } else {
    columns.splice(3, 0, ...otherColumns);
  }

  // 表格改变事件， 手动控制排序规则
  const handleTableChange = (pagination, filter, sorter) => {
    if (sortOrder !== sorter.order) {
      setSortOrder(sorter.order);
    }
  };
  // tab栏目数据
  const parentTabs = [
    { value: '1', label: '待入库' },
    { value: '2', label: '已入库' },
  ];

  const commonTableProps = {
    parentTabs,
    // 顶部搜索表单中的属性，
    searchFormProps: {},
    // 设置表单默认值， 默认查询条件
    searchParams: {
      currentPage: 1,
      orderStage: '',
      pageSize: 10,
      searchCount: true,

      contractNum: '',
      supplierName: '',
      dateRange: {
        startTime,
        endTime,
      },
    },
    dispatch,
    // 表格改变事件
    handleTableChange,
    // tab栏目改变事件
    onTabChange: activeKey => setTabKey(activeKey),
  };

  const rowKey = 'buyProductPendingStorageId';

  // 当tabkey===2 时的  TableWrapper 组件props
  const CusTomTableProps = {
    ...commonTableProps,
    // 表格属性
    tableProps: {
      columns,
      rowKey,
    },
    operateText: '查看已新增入库信息',
    onOperateClick: () => {
      dispatch({
        type: 'purchaseWarehousingModel/getFreeStorageEffect',
        payload: {},
        callback: (_, status, msg) => {
          if (status === 'error') {
            message.error(msg);
          } else {
            setCheckViewConfirmModalVisible(true);
          }
        },
      });
    },
    listService: purchaseStorageService,
  };
  // 当tabkey===1 时的  TableWrapper 组件props
  const CusTomTablePropsForWait = {
    ...commonTableProps,
    tableProps: {
      columns,
      rowKey,
    },

    operateText: '新增入库',
    onOperateClick: () => setConfirmModalVisible(true),
    listService: purchaseStorageWaitService,
  };

  const cacheSaveFreeStorage = field => {
    dispatch({
      type: 'purchaseWarehousingModel/cacheSaveFreeStorageEffect',
      payload: field,
      callback: (_, status, msg) => {
        if (status === 'error') {
          message.error(msg);
        }
      },
    });
  };

  // "新增入库"modal弹窗props
  const ComfirmFormModalProps = {
    visible: confirmModalVisible,
    onOk: () => {
      try {
        _czc1.push(['_trackEvent', '采购入库', '新增', '', '', '']);
      } catch (error) {}
      dispatch({
        type: 'purchaseWarehousingModel/saveFreeStorageEffect',
        payload: {},
        callback: (_, status, msg) => {
          if (status === 'error') {
            message.error(msg);
          } else {
            message.success(msg);
            setConfirmModalVisible(false);
          }
        },
      });
    },
    onCancel: () => setConfirmModalVisible(false),
    title: '确认入库',
    ComProps: {
      storageOptions,
      productsOptions,
      productSpecOptions,
      dataDetail: saveFreeStorageParams,
      onChange: field => cacheSaveFreeStorage(field),
    },
  };

  const myProps = tabKey === '1' ? { ...CusTomTablePropsForWait } : { ...CusTomTableProps };

  const paramsHandler = params => {
    const { dateRange: newdateRange, contractNum, supplierName, ...other } = params;

    const requestParams = {
      startTime: newdateRange.startTime,
      endTime: newdateRange.endTime,
      contractNum,
      supplierName,
      ...other,
    };
    return requestParams;
  };

  // 查看确认新增入库的  详情
  const CheckViewComfirmModalProps = {
    visible: checkViewConfirmModalVisible,
    onOk: () => setCheckViewConfirmModalVisible(false),
    onCancel: () => setCheckViewConfirmModalVisible(false),
    title: '新增入库的详情',

    ComProps: {
      detailDataWithAdd,
    },
  };

  return (
    <>
      <TableWrapper
        {...myProps}
        currentTabKey={tabKey}
        paramsHandler={paramsHandler}
        tableWrapperRef={a => {
          console.log(a);
        }}
      >
        <MyFormSearch />
      </TableWrapper>
      <NewComfirmFormModal {...ComfirmFormModalProps} />
      <CheckViewComfirmModal {...CheckViewComfirmModalProps} />
    </>
  );
};

// 绑定redux
export default connect(state => {
  const {
    storageOptions,
    productsOptions,
    productSpecOptions,
    saveFreeStorageParams,
    detailDataWithAdd,
  } = state.purchaseWarehousingModel;

  return {
    storageOptions,
    productsOptions,
    productSpecOptions,
    saveFreeStorageParams,
    detailDataWithAdd,
  };
})(Index);
