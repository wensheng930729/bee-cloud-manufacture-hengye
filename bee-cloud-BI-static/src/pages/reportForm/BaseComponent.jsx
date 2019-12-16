import React, { Component } from 'react';
import { message, Spin } from 'antd';
import { connect } from 'dva';
import { isEqual, debounce } from 'lodash';
import { reportType as REPORT_TYPE } from '@/consts/reportForm';

@connect(({ ReportFormCommonModel, loading }) => ({
  productsOptions: ReportFormCommonModel.productsOptions,
  storageOptions: ReportFormCommonModel.storageOptions,
  productsCateOptions: ReportFormCommonModel.productsCateOptions,
  columnsObj: ReportFormCommonModel.columnsObj,
  columnsParams: ReportFormCommonModel.columnsParams,

  listSearchParams: ReportFormCommonModel.listSearchParams,
  parentTabs: ReportFormCommonModel.parentTabs,
  dataObj: ReportFormCommonModel.dataObj,
  loading:
    loading.effects['ReportFormCommonModel/getListEffect'] ||
    loading.effects['ReportFormCommonModel/fetchColumnsNameEffect'] ||
    loading.effects['ReportFormCommonModel/getTabPagesEffect'],
}))
class Index extends Component {
  static defaultProps = {
    hasTab: true,
  };

  // 判断是否存在  tab
  static getDerivedStateFromProps(nextProps, prevState) {
    const bool1 = !isEqual(nextProps.parentTabs, prevState.stateTabs);

    if (bool1 && nextProps.hasTab) {
      return {
        stateTabs: nextProps.parentTabs,
      };
    }
    if (!nextProps.hasTab) {
      return {
        stateTabs: [],
      };
    }

    // 否则，对于state不进行任何操作
    return null;
  }

  constructor(props) {
    super(props);
    this.state = {
      stateTabs: props.parentTabs,
    };

    this.handleTabChange = debounce(this.handleTabChange, 300);
  }

  componentDidMount() {
    const { defaultTabPagesParams } = this.props;

    this.getTabPages(defaultTabPagesParams);
  }

  componentWillUnmount() {
    const { productsOptions } = this.props;
    this.handleFormChange({ currentPage: 1, pageSize: 10, productId: productsOptions[0].value });
  }

  // 是否能查看当前页面
  getTabPages = payload => {
    const { dispatch } = this.props;
    dispatch({
      type: 'ReportFormCommonModel/getTabPagesEffect',
      payload,
      callback: (data, status, msg) => {
        if (status === 'error') {
          message.error(msg);
        }
      },
    });
  };

  // 表单查询改变事件
  handleFormChange = field => {
    const { dispatch, listSearchParams } = this.props;
    dispatch({
      type: 'ReportFormCommonModel/cacheSearchParamsEffect',
      payload: { ...listSearchParams, ...field },
    });
    if (field.productId) {
      const data = { productId: field.productId };

      this.getColumns({ listSearchParams: data, columnsParams: data });
    }
  };

  // 搜索按钮 点击事件
  handleSearch = () => {
    this.getTableData({ currentPage: 1, pageSize: 10 });
  };

  // tab切换事件
  handleTabChange = reportType => {
    const {
      columnsParams: { businessType: oldbusinessType },
    } = this.props;
    let businessType = oldbusinessType;
    switch (reportType) {
      // 产成品入库
      case REPORT_TYPE.productWarehouse.key:
        businessType = REPORT_TYPE.productWarehouse.businessType;
        break;
      // 成品出库
      case REPORT_TYPE.productOutStorage.key:
        businessType = REPORT_TYPE.productOutStorage.businessType;
        break;
      // 生产样质检报表
      case REPORT_TYPE.productQualiInspect.key:
        businessType = REPORT_TYPE.productQualiInspect.businessType;
        break;
      // 进出厂质检报表
      case REPORT_TYPE.entryExitQualiInspect.key:
        businessType = REPORT_TYPE.entryExitQualiInspect.businessType;
        break;

      default:
        break;
    }
    this.getColumns({
      listSearchParams: { reportType, currentPage: 1, pageSize: 10 },
      columnsParams: { reportType, businessType },
    });
  };

  // 表格分页事件
  handleTableChange = payload => {
    this.getTableData(payload);
  };

  // 获取查询条件中的 下拉框数据，并设置 默认查询条件
  getSearchOptions = payload => {
    const { dispatch } = this.props;
    dispatch({
      type: 'ReportFormCommonModel/fetchInitEffect',
      payload,
      callback: (data, status, msg) => {
        if (status === 'error') {
          message.error(msg);
        } else if (status === 'ok') {
          const { productsOptions, storageOptions } = data;

          const newlistSearchParams = {
            productId: productsOptions[0].value,
            storageId: storageOptions[0].value,
          };

          this.handleFormChange(newlistSearchParams);

          this.getTableData(newlistSearchParams);
        }
      },
    });
  };

  // 获取列名
  getColumns = payload => {
    const { dispatch } = this.props;
    dispatch({
      type: 'ReportFormCommonModel/fetchColumnsNameEffect',
      payload,
      callback: (data, status, msg) => {
        if (status === 'error') {
          message.error(msg);
        }
      },
    });
  };

  // 获取表格数据
  getTableData = (obj = {}) => {
    // 表格之前的查询条件
    const { dispatch, listSearchParams } = this.props;
    dispatch({
      type: 'ReportFormCommonModel/getListEffect',
      payload: {
        reportType: listSearchParams.reportType,
        ...obj,
      },
      callback: (data, status, msg) => {
        if (status === 'error') {
          message.error(msg);
        }
      },
    });
  };

  render() {
    const { stateTabs } = this.state;
    const {
      productsOptions,
      storageOptions,
      productsCateOptions,
      loading,
      listSearchParams,
      columnsObj,
      dataObj,
      children,
      parentTabs,
    } = this.props;

    if (parentTabs.length === 0) {
      return <h1 style={{ marginLeft: '40%' }}>请联系管理员开启当前页面</h1>;
    }

    const searchFormProps = {
      productsOptions,
      storageOptions,
      productsCateOptions,
      nostorageId: listSearchParams.reportType === REPORT_TYPE.rawMaterial.key,
      hasCate: listSearchParams.reportType === REPORT_TYPE.entryExitQualiInspect.key,
      islogisticsSale: listSearchParams.reportType === REPORT_TYPE.logisticsSale.key,
      dataDetail: listSearchParams,
      handleSearch: this.handleSearch,
      onChange: this.handleFormChange,
    };

    const selfProps = {
      loading,
      columns: columnsObj[listSearchParams.reportType] || [],
      data: dataObj[listSearchParams.reportType] || {
        list: [],
        pagination: {},
      },

      parentTabs: stateTabs,
      currentTab: listSearchParams.reportType,
      handleTabChange: this.handleTabChange,
      handleTableChange: this.handleTableChange,

      ...searchFormProps,
    };
    return <Spin spinning={loading}>{React.cloneElement(children, { ...selfProps })}</Spin>;
  }
}

export default Index;
